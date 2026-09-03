(* Prices [Ucharset.Partition.meet_all] at the shapes
   [approx_partition] builds it from.

   Run: dune exec --profile release bench/part_bench.exe *)

(* Each leaf contributes [of_set c], the two block partition
   {c, comp c} covering the codespace, and an [Alt] of n leaves meets
   all n of them. *)
let sets_of_n n offset =
  List.init n (fun i ->
    Ucharset.range ~lo:(0x20000 + (i * 8) + offset) ~hi:(0x20000 + (i * 8) + 3 + offset))
;;

let bench_meet_all n =
  let ps = List.map Ucharset.Partition.of_set (sets_of_n n 0) in
  let iters = max 100 (100_000 / n) in
  let t0 = Unix.gettimeofday () in
  let acc = ref 0 in
  for _ = 1 to iters do
    acc := !acc + Ucharset.Partition.num_blocks (Ucharset.Partition.meet_all ps)
  done;
  let dt = Unix.gettimeofday () -. t0 in
  let meets = n - 1 in
  Printf.printf
    "  %8d %10d %14.0f %14.0f\n%!"
    n
    (Ucharset.Partition.num_blocks (Ucharset.Partition.meet_all ps))
    (dt *. 1e9 /. float_of_int iters)
    (dt *. 1e9 /. float_of_int iters /. float_of_int (max 1 meets));
  ignore !acc
;;

let () =
  Printf.printf "Partition.meet_all over n leaf partitions (the Alt case)\n\n";
  Printf.printf "  %8s %10s %14s %14s\n" "n" "blocks out" "ns/meet_all" "ns/meet";
  List.iter bench_meet_all [ 2; 4; 8; 16; 32; 64; 128; 256 ]
;;
