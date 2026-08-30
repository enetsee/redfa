(* Prices [Ucharset.mem], which the [first_set] guard in [deriv] pays
   on every child it prunes. The interval counts run from a single
   range to one per rule of a wide alternation.

   Run: dune exec --profile release bench/mem_bench.exe *)

(* [n] disjoint 4-wide ranges, spaced so they never coalesce. *)
let set_of_n n =
  Ucharset.union_list (List.init n (fun i -> Ucharset.range ~lo:(0x20000 + (i * 8)) ~hi:(0x20000 + (i * 8) + 3)))
;;

(* Probes that mostly miss, which is the pruned-guard case. *)
let probes = Array.init 4096 (fun i -> 0x20000 + (i * 5) + 4)

let bench n =
  let s = set_of_n n in
  let iters = 400 in
  let hits = ref 0 in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to iters do
    Array.iter (fun cp -> if Ucharset.mem s cp then incr hits) probes
  done;
  let dt = Unix.gettimeofday () -. t0 in
  let calls = iters * Array.length probes in
  Printf.printf
    "  %8d %10d %12.2f\n%!"
    n
    (Ucharset.cardinal s)
    (dt *. 1e9 /. float_of_int calls);
  ignore !hits
;;

let () =
  Printf.printf "Ucharset.mem cost by interval count\n\n";
  Printf.printf "  %8s %10s %12s\n" "ivals" "cardinal" "ns/call";
  List.iter bench [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]
;;
