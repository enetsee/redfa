(* Scales [deriv] along depth (the [seq_suffix] rebuild in
   [deriv_seq]) and width (the fan-out over [Alt] children) using one
   [rich k n] family, so only one varies at a time. Reports ns per
   derivative, which divides out state count growth.

   Run: dune exec --profile release bench/scale_bench.exe *)

open Redfa.Ast

let rich k b =
  alts
    (List.init k (fun i ->
       let base = 0x20000 + (i * ((8 * b) + 8)) in
       let cls j = range ~lo:(base + (j * 8)) ~hi:(base + (j * 8) + 3) in
       seqs (List.init (b - 1) (fun j -> opt (cls j)) @ [ plus (cls (b - 1)) ])))
;;

module H = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal
    let hash = hash
  end)

let build start =
  let seen = H.create 1024 in
  let n_deriv = ref 0 in
  let rec go r =
    if not (H.mem seen r)
    then (
      H.add seen r ();
      List.iter
        (fun cp ->
           incr n_deriv;
           go (deriv r ~uchr:cp))
        (approx_representatives r))
  in
  go start;
  H.length seen, !n_deriv
;;

let measure k n =
  let r = rich k n in
  let t0 = Unix.gettimeofday () in
  let states, derivs = build r in
  let ms = (Unix.gettimeofday () -. t0) *. 1000. in
  Printf.printf "%.6f %d %d\n" ms states derivs
;;

let child k n =
  let rd, wr = Unix.pipe () in
  let argv = [| Sys.executable_name; string_of_int k; string_of_int n |] in
  let pid = Unix.create_process Sys.executable_name argv Unix.stdin wr Unix.stderr in
  Unix.close wr;
  let ic = Unix.in_channel_of_descr rd in
  let line = input_line ic in
  close_in ic;
  ignore (Unix.waitpid [] pid);
  Scanf.sscanf line "%f %d %d" (fun m s d -> m, s, d)
;;

(* Best-of, on total ms; ns/deriv is derived from the winning run. *)
let best reps k n =
  let rec go i (bm, bs, bd) =
    if i = 0
    then bm, bs, bd
    else (
      let m, s, d = child k n in
      go (i - 1) (if m < bm then m, s, d else bm, bs, bd))
  in
  go reps (infinity, 0, 0)
;;

let row reps k n =
  let ms, states, derivs = best reps k n in
  let ns_per = ms *. 1e6 /. float_of_int derivs in
  Printf.printf "  %6d %6d %10.3f %8d %9d %11.1f\n%!" k n ms states derivs ns_per
;;

let () =
  if Array.length Sys.argv = 3
  then measure (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2))
  else (
    let reps = 7 in
    let hdr () =
      Printf.printf
        "  %6s %6s %10s %8s %9s %11s\n"
        "k"
        "N"
        "ms"
        "states"
        "derivs"
        "ns/deriv"
    in
    Printf.printf "depth scaling: k fixed at 32, N doubling\n\n";
    hdr ();
    List.iter (fun n -> row reps 32 n) [ 2; 4; 8; 16; 32; 64 ];
    Printf.printf "\nwidth scaling: N fixed at 4, k doubling\n\n";
    hdr ();
    List.iter (fun k -> row reps k 4) [ 8; 16; 32; 64; 128; 256 ])
;;
