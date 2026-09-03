(* What a code generator emits against, for the two views in [Dfa]'s
   emission section.

   A generator turns each state's [transitions] into a chain of
   interval tests, and emits that chain once per range it has a fast
   path for (typically one for the byte that is its own codepoint in
   UTF-8, one for the rest). The "arms x2" column is what that costs
   with the whole dispatch on both sides; "split" is the same with
   [transitions_in] asked once per range.

   [Dfa.table] is the other view; the classes the automaton
   distinguishes, and a states by classes array. Emitting it is a
   class set and a cell per pair, against an arm per transition.

   These are code sizes rather than timings. The one time here is what
   [Dfa.table] costs to build at codegen time.

   Run: dune exec --profile release bench/emit_bench.exe *)

open Redfa

let clock f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (Unix.gettimeofday () -. t0) *. 1000., r
;;

let row label toks =
  let dfa = Dfa.of_tokens toks in
  let n = Dfa.num_states dfa in
  let build_ms, tbl = clock (fun () -> Dfa.table dfa) in
  let k = Array.length tbl.Dfa.classes in
  let arms = ref 0
  and ivals = ref 0
  and split = ref 0 in
  Dfa.iter_states dfa (fun id ->
    let ts = Dfa.transitions dfa id in
    arms := !arms + List.length ts;
    List.iter (fun (cs, _) -> ivals := !ivals + Ucharset.num_intervals cs) ts;
    split
    := !split
       + List.length (Dfa.transitions_in dfa id ~lo:0 ~hi:0x7F)
       + List.length (Dfa.transitions_in dfa id ~lo:0x80 ~hi:0x10FFFF));
  let packed =
    Array.fold_left
      (fun a cs -> a + String.length (Ucharset.to_packed_string cs))
      0
      tbl.Dfa.classes
  in
  Printf.printf
    "  %-14s %7d %8d %9d %8d %9d %8d %9d %9.1f\n%!"
    label
    n
    (2 * !arms)
    (2 * !ivals)
    !split
    k
    (n * k)
    packed
    build_ms
;;

let () =
  Printf.printf
    "  %-14s %7s %8s %9s %8s %9s %8s %9s %9s\n"
    "workload"
    "states"
    "arms x2"
    "ivals x2"
    "split"
    "classes"
    "cells"
    "packed B"
    "build ms";
  List.iter
    (fun (name, toks) -> row name (List.mapi (fun i r -> i, r) (toks ())))
    Workloads.all
;;
