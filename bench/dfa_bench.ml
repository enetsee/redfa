(* Benchmarks [Dfa.of_tokens] and [Dfa.minimise]. Tokens go in as
   separate rules, the path that exercises the multi-item joint
   partition. Fresh child process per measurement, the hash-cons table
   and approx memo being global.

   Run: dune exec --profile release bench/dfa_bench.exe *)

open Redfa

let workloads = Workloads.all

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
