(* The token sets the benchmarks are measured on, shared so that
   "the same shapes" means the same shapes. [dfa_bench] builds
   automata from them; [equiv_bench] decides languages over them. *)

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

let all =
  [ "ascii", ascii_tokens
  ; ("keyword-400", fun () -> keyword_tokens 400)
  ; ("rich-128x8", fun () -> rich_tokens 128 8)
  ; ("boolean-64", fun () -> boolean_tokens 64)
  ]
;;
