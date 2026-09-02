(* Benchmarks [Dfa.of_tokens] and [Dfa.minimise]. Tokens go in as
   separate rules, the path that exercises the multi-item joint
   partition. Fresh child process per measurement, the hash-cons table
   and approx memo being global.

   Run: dune exec --profile release bench/dfa_bench.exe *)

open Redfa

let sc c = Regex.chars (Ucharset.singleton_char c)
let rng a b = Regex.range ~lo:(Char.code a) ~hi:(Char.code b)
let of_s s = Regex.chars (Ucharset.of_utf_8_string s)

let ascii_tokens () =
  let open Regex in
  let lower = rng 'a' 'z'
  and upper = rng 'A' 'Z'
  and digit = rng '0' '9' in
  let alpha = alt lower upper in
  let alnum = alts [ alpha; digit; sc '_' ] in
  [ seq (alt alpha (sc '_')) (star alnum)
  ; plus digit
  ; seqs
      [ plus digit
      ; sc '.'
      ; star digit
      ; opt (seqs [ of_s "eE"; opt (of_s "+-"); plus digit ])
      ]
  ; seqs
      [ sc '"'
      ; star
          (alt
             (chars
                (Ucharset.diff Ucharset.all ~remove:(Ucharset.of_utf_8_string "\"\\")))
             (seq (sc '\\') any))
      ; sc '"'
      ]
  ; seqs
      [ str "//"
      ; star (chars (Ucharset.diff Ucharset.all ~remove:(Ucharset.singleton_char '\n')))
      ]
  ; plus (of_s " \t\r\n")
  ; alts
      (List.map str [ "->"; "<-"; ":="; "=="; "!="; "<="; ">="; "&&"; "||"; "|>"; "::" ])
  ; alts
      (List.map
         sc
         [ '('; ')'; '['; ']'; '{'; '}'; ','; ';'; ':'; '.'; '+'; '-'; '*'; '/' ])
  ]
;;

let keyword_tokens n =
  let st = Random.State.make [| 7 |] in
  let word () =
    String.init
      (3 + Random.State.int st 6)
      (fun _ -> Char.chr (Char.code 'a' + Random.State.int st 26))
  in
  ascii_tokens () @ List.init n (fun _ -> Regex.str (word ()))
;;

let rich_tokens k b =
  let open Regex in
  List.init k (fun i ->
    let base = 0x20000 + (i * ((8 * b) + 8)) in
    let cls j = range ~lo:(base + (j * 8)) ~hi:(base + (j * 8) + 3) in
    seqs (List.init (b - 1) (fun j -> opt (cls j)) @ [ plus (cls (b - 1)) ]))
;;

let boolean_tokens k =
  let open Regex in
  List.init k (fun i ->
    let base = 0x20000 + (i * 256) in
    let cls j = range ~lo:(base + (j * 16)) ~hi:(base + (j * 16) + 7) in
    inters
      [ star (alts [ cls 0; cls 1; cls 2; cls 3 ])
      ; complement (seqs [ cls 1; star (cls 2) ])
      ; star (alts [ cls 0; cls 2; cls 4; cls 5 ])
      ])
;;

let workloads =
  [ "ascii", ascii_tokens
  ; ("keyword-400", fun () -> keyword_tokens 400)
  ; ("rich-128x8", fun () -> rich_tokens 128 8)
  ; ("boolean-64", fun () -> boolean_tokens 64)
  ]
;;

let clock f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (Unix.gettimeofday () -. t0) *. 1000., r
;;

let numbered rs = List.mapi (fun i r -> i, r) rs

(* The counts a code generator emits against: one arm per transition,
   and the intervals of each arm's charset. The last column gives the
   same counts with transitions coalesced by destination. *)
let shape_of workload =
  let toks = numbered ((List.assoc workload workloads) ()) in
  let dfa = Dfa.of_tokens toks in
  let edges = ref 0
  and ivals = ref 0
  and cedges = ref 0
  and civals = ref 0 in
  Dfa.iter_states dfa (fun id ->
    let ts = Dfa.transitions dfa id in
    edges := !edges + List.length ts;
    List.iter (fun (cs, _) -> ivals := !ivals + Ucharset.num_intervals cs) ts;
    (* coalesce by destination *)
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (cs, d) ->
         let prev =
           try Hashtbl.find tbl d with
           | Not_found -> []
         in
         Hashtbl.replace tbl d (cs :: prev))
      ts;
    Hashtbl.iter
      (fun _ css ->
         incr cedges;
         let u =
           match css with
           | [ c ] -> c
           | cs -> Ucharset.union_list cs
         in
         civals := !civals + Ucharset.num_intervals u)
      tbl);
  Printf.printf
    "  %-14s %8d %8d %8d %10d %10d\n%!"
    workload
    (Dfa.num_states dfa)
    !edges
    !cedges
    !ivals
    !civals
;;

let measure workload =
  let toks = numbered ((List.assoc workload workloads) ()) in
  let build_ms, dfa = clock (fun () -> Dfa.of_tokens toks) in
  let min_ms, mini = clock (fun () -> Dfa.minimise dfa) in
  let edges =
    let n = ref 0 in
    Dfa.iter_states dfa (fun id -> n := !n + List.length (Dfa.transitions dfa id));
    !n
  in
  Printf.printf
    "%.4f %.4f %d %d %d\n"
    build_ms
    min_ms
    (Dfa.num_states dfa)
    (Dfa.num_states mini)
    edges
;;

let child workload =
  let rd, wr = Unix.pipe () in
  let pid =
    Unix.create_process
      Sys.executable_name
      [| Sys.executable_name; workload |]
      Unix.stdin
      wr
      Unix.stderr
  in
  Unix.close wr;
  let ic = Unix.in_channel_of_descr rd in
  let line = input_line ic in
  close_in ic;
  ignore (Unix.waitpid [] pid);
  Scanf.sscanf line "%f %f %d %d %d" (fun b m s ms e -> b, m, s, ms, e)
;;

let best reps workload =
  let rec go i acc =
    if i = 0
    then acc
    else (
      let ((b, m, _, _, _) as r) = child workload in
      let bb, bm, _, _, _ = acc in
      go (i - 1) (if b +. m < bb +. bm then r else acc))
  in
  go reps (infinity, infinity, 0, 0, 0)
;;

let () =
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "shape"
  then shape_of Sys.argv.(2)
  else if Array.length Sys.argv = 2
  then measure Sys.argv.(1)
  else (
    let reps = 7 in
    Printf.printf
      "Dfa.of_tokens / Dfa.minimise (ms, best of %d, fresh process each)\n\n"
      reps;
    Printf.printf
      "  %-14s %10s %10s %8s %8s %8s\n"
      "workload"
      "build"
      "minimise"
      "states"
      "min"
      "edges";
    List.iter
      (fun (name, _) ->
         let b, m, s, ms, e = best reps name in
         Printf.printf "  %-14s %10.3f %10.3f %8d %8d %8d\n%!" name b m s ms e)
      workloads)
;;
