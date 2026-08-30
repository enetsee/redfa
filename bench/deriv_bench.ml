(* Benchmarks the derivative and full DFA construction.

   Run: dune exec --profile release bench/deriv_bench.exe

   Fresh child process per measurement: the hash-cons table and the
   per-node approx memo are global. *)

open Redfa.Ast

let sc c = chars (Ucharset.singleton_char c)
let rng a b = range ~lo:(Char.code a) ~hi:(Char.code b)

let ascii_lexer () =
  let lower = rng 'a' 'z'
  and upper = rng 'A' 'Z'
  and digit = rng '0' '9' in
  let alpha = alt lower upper in
  let alnum = alts [ alpha; digit; sc '_' ] in
  let of_s s = chars (Ucharset.of_utf_8_string s) in
  alts
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
        (List.map
           str
           [ "->"; "<-"; ":="; "=="; "!="; "<="; ">="; "&&"; "||"; "|>"; "::" ])
    ; alts
        (List.map
           sc
           [ '('; ')'; '['; ']'; '{'; '}'; ','; ';'; ':'; '.'; '+'; '-'; '*'; '/' ])
    ]
;;

let keyword_lexer n =
  let st = Random.State.make [| 7 |] in
  let word () =
    String.init
      (3 + Random.State.int st 6)
      (fun _ -> Char.chr (Char.code 'a' + Random.State.int st 26))
  in
  alts (ascii_lexer () :: List.init n (fun _ -> str (word ())))
;;

let rich k b =
  alts
    (List.init k (fun i ->
       let base = 0x20000 + (i * ((8 * b) + 8)) in
       let cls j = range ~lo:(base + (j * 8)) ~hi:(base + (j * 8) + 3) in
       seqs (List.init (b - 1) (fun j -> opt (cls j)) @ [ plus (cls (b - 1)) ])))
;;

(* A boolean shape: intersections and complements. *)
let boolean k =
  alts
    (List.init k (fun i ->
       let base = 0x20000 + (i * 256) in
       let cls j = range ~lo:(base + (j * 16)) ~hi:(base + (j * 16) + 7) in
       inters
         [ star (alts [ cls 0; cls 1; cls 2; cls 3 ])
         ; complement (seqs [ cls 1; star (cls 2) ])
         ; star (alts [ cls 0; cls 2; cls 4; cls 5 ])
         ]))
;;

let workloads =
  [ ("ascii", fun () -> ascii_lexer ())
  ; ("keyword-400", fun () -> keyword_lexer 400)
  ; ("rich-128x8", fun () -> rich 128 8)
  ; ("boolean-64", fun () -> boolean 64)
  ]
;;

module H = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal
    let hash = hash
  end)

let clock f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (Unix.gettimeofday () -. t0) *. 1000., r
;;

(* Full DFA construction, and the same traversal with the two phases timed
   separately so their shares are visible. *)
let build ?(split = false) start =
  let seen = H.create 1024 in
  let n_deriv = ref 0
  and t_ap = ref 0.
  and t_dv = ref 0. in
  let rec go r =
    if not (H.mem seen r)
    then (
      H.add seen r ();
      let reps =
        if split
        then (
          let ms, v = clock (fun () -> approx_representatives r) in
          t_ap := !t_ap +. ms;
          v)
        else approx_representatives r
      in
      List.iter
        (fun cp ->
           incr n_deriv;
           let d =
             if split
             then (
               let ms, v = clock (fun () -> deriv r ~uchr:cp) in
               t_dv := !t_dv +. ms;
               v)
             else deriv r ~uchr:cp
           in
           go d)
        reps)
  in
  go start;
  H.length seen, !n_deriv, !t_ap, !t_dv
;;

let measure mode workload =
  let r = (List.assoc workload workloads) () in
  match mode with
  | "build" ->
    let ms, (states, edges, _, _) = clock (fun () -> build r) in
    Printf.printf "%.4f %d %d\n" ms states edges
  | "split" ->
    let _, (states, edges, ap, dv) = clock (fun () -> build ~split:true r) in
    Printf.printf "%.4f %.4f %d %d\n" ap dv states edges
  | _ -> failwith "mode"
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
  line
;;

let best reps mode workload sel =
  let rec go i acc line =
    if i = 0
    then acc, line
    else (
      let l = child mode workload in
      let v = sel l in
      if v < acc then go (i - 1) v l else go (i - 1) acc line)
  in
  go reps infinity ""
;;

let () =
  if Array.length Sys.argv = 3
  then measure Sys.argv.(1) Sys.argv.(2)
  else (
    let reps = 11 in
    Printf.printf "full DFA construction (ms, best of %d, fresh process each)\n\n" reps;
    Printf.printf "  %-14s %10s %10s %10s\n" "workload" "total" "states" "derivs";
    List.iter
      (fun (name, _) ->
         let ms, line =
           best reps "build" name (fun l -> Scanf.sscanf l "%f %d %d" (fun m _ _ -> m))
         in
         Scanf.sscanf line "%f %d %d" (fun _ s e ->
           Printf.printf "  %-14s %10.3f %10d %10d\n%!" name ms s e))
      workloads;
    Printf.printf "\nwhere it goes (ms, instrumented so totals exceed the above)\n\n";
    Printf.printf "  %-14s %10s %10s %8s\n" "workload" "approx" "deriv" "deriv %";
    List.iter
      (fun (name, _) ->
         let _, line =
           best reps "split" name (fun l ->
             Scanf.sscanf l "%f %f %d %d" (fun a d _ _ -> a +. d))
         in
         Scanf.sscanf line "%f %f %d %d" (fun a d _ _ ->
           Printf.printf
             "  %-14s %10.3f %10.3f %7.0f%%\n%!"
             name
             a
             d
             (100. *. d /. (a +. d))))
      workloads)
;;
