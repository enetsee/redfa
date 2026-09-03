(* [Ast.equivalent] and [Ast.is_empty_language] against the two
   alternatives they were chosen over: the same traversal without the
   union-find, which is the reachable product, and deciding on an
   automaton from [Dfa.of_tokens].

   Each workload is the alternation of its tokens against the same
   alternation with every arm intersected with [.*]: same language,
   different term at every node.

   One variant per child process. [first_set] and [approx_partition]
   memoise on the node, so whichever runs first pays for both and the
   rest read the memos -- in one process the second is two to six
   times faster than the first, whichever one it is.

   Run: dune exec --profile release bench/equiv_bench.exe *)

open Redfa

(* Without the union-find: pairs are visited once and remembered,
   which is a reachability search over the product automaton. Returns
   the answer and the pairs it expanded. *)
let product_equivalent a b =
  let seen : (int * int, unit) Hashtbl.t = Hashtbl.create 256 in
  let pairs = ref 0 in
  let rec go = function
    | [] -> true
    | (x, y) :: rest ->
      let k = Ast.tag x, Ast.tag y in
      if Hashtbl.mem seen k
      then go rest
      else (
        Hashtbl.add seen k ();
        incr pairs;
        if Ast.is_nullable x <> Ast.is_nullable y
        then false
        else (
          let p =
            Ucharset.Partition.meet (Ast.approx_partition x) (Ast.approx_partition y)
          in
          let todo = ref rest in
          for i = Ucharset.Partition.num_blocks p - 1 downto 0 do
            let c = Ucharset.Partition.representative p i in
            let dx = Ast.deriv x ~uchr:c
            and dy = Ast.deriv y ~uchr:c in
            if not (Ast.equal dx dy) then todo := (dx, dy) :: !todo
          done;
          go !todo))
  in
  let r = go [ a, b ] in
  r, !pairs
;;

(* A copy of [Ast.equivalent], here only to count the pairs it
   expands; the timing column comes from the library. *)
let hk_pairs a b =
  let parent : (int, int) Hashtbl.t = Hashtbl.create 256 in
  let size : (int, int) Hashtbl.t = Hashtbl.create 256 in
  let rec find x =
    match Hashtbl.find_opt parent x with
    | None -> x
    | Some p ->
      let r = find p in
      if r <> p then Hashtbl.replace parent x r;
      r
  in
  let sz x =
    match Hashtbl.find_opt size x with
    | Some s -> s
    | None -> 1
  in
  let union a b =
    let sa = sz a
    and sb = sz b in
    let big, small = if sa >= sb then a, b else b, a in
    Hashtbl.replace parent small big;
    Hashtbl.replace size big (sa + sb)
  in
  let pairs = ref 0 in
  let rec go = function
    | [] -> true
    | (x, y) :: rest ->
      let rx = find (Ast.tag x)
      and ry = find (Ast.tag y) in
      if rx = ry
      then go rest
      else if Ast.is_nullable x <> Ast.is_nullable y
      then false
      else (
        union rx ry;
        incr pairs;
        let p =
          Ucharset.Partition.meet (Ast.approx_partition x) (Ast.approx_partition y)
        in
        let todo = ref rest in
        for i = Ucharset.Partition.num_blocks p - 1 downto 0 do
          let c = Ucharset.Partition.representative p i in
          let dx = Ast.deriv x ~uchr:c
          and dy = Ast.deriv y ~uchr:c in
          if not (Ast.equal dx dy) then todo := (dx, dy) :: !todo
        done;
        go !todo)
  in
  let r = if Ast.equal a b then true else go [ a, b ] in
  r, !pairs
;;

(* Deciding on the automaton instead. Both terms go in as tokens of
   one DFA, whose states are the pairs of residuals reachable
   together; a state accepting one and not the other is a separating
   string. *)
let dfa_equivalent r1 r2 =
  let d = Dfa.of_tokens [ 0, r1; 1, r2 ] in
  let agree = ref true in
  Dfa.iter_states d (fun id ->
    match Dfa.accepts d id with
    | [ 0 ] | [ 1 ] -> agree := false
    | _ -> ());
  !agree, Dfa.num_states d
;;

(* [(a{p})*a*] is [a*] written as a p-state cycle. Two of them with
   coprime lengths are the same language through automata that share
   no state, so the product visits [p * q] pairs where the union-find
   visits [p + q]. The adversarial shape, against the four realistic
   ones. *)
let cycle p =
  let open Regex in
  let a = singleton_char 'a' in
  seq (star (seqs (List.init p (fun _ -> a)))) (star a)
;;

(* One regex per workload, and an equivalent term the normal form
   cannot see through: every arm intersected with [.*]. *)
let pair_of workload =
  if workload = "cycles-63x64"
  then cycle 63, cycle 64
  else (
    let toks = (List.assoc workload Workloads.all) () in
    ( Regex.alts toks
    , Regex.alts (List.map (fun r -> Regex.inter r (Regex.star Regex.any)) toks) ))
;;

let names = List.map fst Workloads.all @ [ "cycles-63x64" ]

let clock f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (Unix.gettimeofday () -. t0) *. 1000., r
;;

(* One variant, in a process of its own: milliseconds, a count of
   whatever the variant explores, and whether it answered correctly.
   Counting the pairs is done after the clock stops. *)
let measure mode workload =
  let r1, r2 = pair_of workload in
  let ms, count, ok =
    match mode with
    | "hk" ->
      let a1 = Regex.to_ast r1
      and a2 = Regex.to_ast r2 in
      let ms, r = clock (fun () -> Ast.equivalent a1 a2) in
      let _, n = hk_pairs a1 a2 in
      ms, n, r
    | "product" ->
      let a1 = Regex.to_ast r1
      and a2 = Regex.to_ast r2 in
      let ms, (r, n) = clock (fun () -> product_equivalent a1 a2) in
      ms, n, r
    | "dfa" ->
      let ms, (r, n) = clock (fun () -> dfa_equivalent r1 r2) in
      ms, n, r
    | "empty-deriv" ->
      let a = Regex.to_ast (Regex.inter r1 (Regex.complement r1)) in
      let ms, r = clock (fun () -> Ast.is_empty_language a) in
      ms, 0, r
    | "empty-dfa" ->
      let term = Regex.inter r1 (Regex.complement r1) in
      let ms, (r, n) =
        clock (fun () ->
          let m = Dfa.minimise (Dfa.of_tokens [ 0, term ]) in
          Dfa.num_states m = 1 && Dfa.is_dead m 0, Dfa.num_states m)
      in
      ms, n, r
    | "empty-live" ->
      let a = Regex.to_ast r1 in
      let ms, r = clock (fun () -> Ast.is_empty_language a) in
      ms, 0, not r
    | m -> failwith ("unknown mode " ^ m)
  in
  Printf.printf "%.4f %d %d\n" ms count (if ok then 1 else 0)
;;

let child mode workload =
  let rd, wr = Unix.pipe () in
  let pid =
    Unix.create_process
      Sys.executable_name
      [| Sys.executable_name; mode; workload |]
      Unix.stdin
      wr
      Unix.stderr
  in
  Unix.close wr;
  let ic = Unix.in_channel_of_descr rd in
  let line = input_line ic in
  close_in ic;
  ignore (Unix.waitpid [] pid);
  Scanf.sscanf line "%f %d %d" (fun ms n ok -> ms, n, ok = 1)
;;

(* Best of [reps], each in its own process. *)
let best reps mode workload =
  let rec go i acc =
    if i = 0
    then acc
    else (
      let ((ms, _, _) as r) = child mode workload in
      let bms, _, _ = acc in
      go (i - 1) (if ms < bms then r else acc))
  in
  go reps (infinity, 0, false)
;;

let reps = 7

let () =
  match Sys.argv with
  | [| _; mode; w |] -> measure mode w
  | _ ->
    let row w =
      let hk, hk_n, hk_ok = best reps "hk" w in
      let pr, pr_n, pr_ok = best reps "product" w in
      let df, df_n, df_ok = best reps "dfa" w in
      Printf.printf
        "  %-14s %10.3f %10.3f %10.3f %9d %9d %9d %6s\n%!"
        w
        hk
        pr
        df
        hk_n
        pr_n
        df_n
        (if hk_ok && pr_ok && df_ok then "yes" else "NO")
    in
    Printf.printf
      "Deciding [r] against an equivalent rewriting of it (ms, best of %d, fresh process \
       each)\n\n"
      reps;
    Printf.printf
      "  %-14s %10s %10s %10s %9s %9s %9s %6s\n"
      "workload"
      "union-find"
      "product"
      "via Dfa"
      "uf pairs"
      "pr pairs"
      "states"
      "agree";
    List.iter row names;
    let erow w =
      let wk, _, wk_ok = best reps "empty-deriv" w in
      let df, df_n, df_ok = best reps "empty-dfa" w in
      let lv, _, lv_ok = best reps "empty-live" w in
      Printf.printf
        "  %-14s %10.3f %10.3f %9d %10.3f %6s\n%!"
        w
        wk
        df
        df_n
        lv
        (if wk_ok && df_ok && lv_ok then "yes" else "NO")
    in
    Printf.printf "\nis_empty_language (ms, best of %d, fresh process each)\n\n" reps;
    Printf.printf
      "  %-14s %10s %10s %9s %10s %6s\n"
      "workload"
      "derivatives"
      "via Dfa"
      "states"
      "non-empty"
      "agree";
    List.iter erow names
;;
