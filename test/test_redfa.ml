(* Property tests for the regex engine and the DFA built from it.

   The generator returns a regex together with an oracle, a plain
   predicate over strings built from the same random choices. The
   oracle shares no code with the engine, so a disagreement is a real
   defect. *)

open Redfa.Regex
module Ast = Redfa.Ast

(* -- oracle combinators ---------------------------------------------------- *)

let match_seq oa ob s =
  let n = String.length s in
  let rec split k =
    k <= n && ((oa (String.sub s 0 k) && ob (String.sub s k (n - k))) || split (k + 1))
  in
  split 0
;;

(* Splits from 1, so an oracle matching the empty string cannot loop. *)
let rec match_star o s =
  String.length s = 0
  ||
  let n = String.length s in
  let rec split k =
    k <= n
    && ((o (String.sub s 0 k) && match_star o (String.sub s k (n - k))) || split (k + 1))
  in
  split 1
;;

(* -- generator ------------------------------------------------------------- *)

(* Four codepoints, so subterms collide constantly and the generated
   terms exercise the dedup and merge paths. The oracle compares bytes,
   so this alphabet has to stay single byte; [meta_alphabet] below is
   for the tests that check a term against itself rather than a
   corpus. *)
let alphabet = [| 0x61; 0x62; 0x63; 0x64 |]

(* Every character redfa's own grammar reserves, plus the control and
   non-BMP codepoints the escaping paths special case. A rendering that
   drops an escape reparses as a different term, which is invisible over
   an alphabet of plain letters. *)
let meta_alphabet =
  [| Char.code '-'
   ; Char.code ']'
   ; Char.code '['
   ; Char.code '^'
   ; Char.code '|'
   ; Char.code '.'
   ; Char.code '~'
   ; Char.code '&'
   ; Char.code '('
   ; Char.code ')'
   ; Char.code '*'
   ; Char.code '+'
   ; Char.code '?'
   ; Char.code '\\'
   ; Char.code ':'
   ; Char.code '{'
   ; Char.code '}'
   ; Char.code '$'
   ; Char.code ' '
   ; 0x00 (* NUL *)
   ; 0x09 (* tab *)
   ; 0x0A (* newline *)
   ; 0x7F (* DEL *)
   ; 0x3BB (* two byte *)
   ; 0xD7FF (* just below the surrogates *)
   ; 0xE000 (* just above them *)
   ; 0x10400 (* supplementary plane, four bytes *)
  |]
;;

let rec gen ~alphabet st depth =
  let leaf () =
    match Random.State.int st 5 with
    | 0 -> empty, fun _ -> false
    | 1 -> eps, fun s -> String.length s = 0
    | 2 -> any, fun s -> String.length s = 1
    | 3 ->
      let c = alphabet.(Random.State.int st (Array.length alphabet)) in
      not_singleton c, fun s -> String.length s = 1 && Char.code s.[0] <> c
    | _ ->
      let c = alphabet.(Random.State.int st (Array.length alphabet)) in
      singleton c, fun s -> String.length s = 1 && Char.code s.[0] = c
  in
  if depth <= 0
  then leaf ()
  else (
    let sub () = gen ~alphabet st (depth - 1) in
    match Random.State.int st 9 with
    | 0 | 1 -> leaf ()
    | 2 ->
      let ra, oa = sub () in
      let rb, ob = sub () in
      seq ra rb, match_seq oa ob
    | 3 ->
      let ra, oa = sub () in
      let rb, ob = sub () in
      alt ra rb, fun s -> oa s || ob s
    | 4 ->
      let ra, oa = sub () in
      let rb, ob = sub () in
      inter ra rb, fun s -> oa s && ob s
    | 5 ->
      let ra, oa = sub () in
      star ra, match_star oa
    | 6 ->
      let ra, oa = sub () in
      (* Complement is over the whole codespace; every string tested
         here is over [alphabet], where negating the oracle agrees. *)
      complement ra, fun s -> not (oa s)
    | 7 ->
      let ra, oa = sub () in
      opt ra, fun s -> String.length s = 0 || oa s
    | _ ->
      let ra, oa = sub () in
      plus ra, match_seq oa (match_star oa))
;;

(* Every string over [alphabet] up to length 3. *)
let corpus =
  let rec grow acc len =
    if len = 0
    then acc
    else (
      let longer =
        List.concat_map
          (fun s ->
             Array.to_list (Array.map (fun c -> s ^ String.make 1 (Char.chr c)) alphabet))
          acc
      in
      acc @ grow longer (len - 1))
  in
  grow [ "" ] 3 |> List.sort_uniq String.compare
;;

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.printf "FAIL: %s\n" name)
;;

(* -- what [Ast]'s exported identity means ---------------------------------- *)

(* [tag] is handed out in allocation order and [compare] and [hash] are
   built from it, so rank is interning order and says nothing about
   structure. First in the file, so these codepoints are certainly
   being interned here for the first time. *)
let () =
  let first = Ast.singleton 0x2F800 in
  let second = Ast.singleton 0x2F801 in
  let third = Ast.singleton 0x2F7FF in
  check "tags ascend with interning order" (Ast.tag first < Ast.tag second);
  check "compare follows the tags" (Ast.compare first second < 0);
  check "hash is the tag" (Ast.hash first = Ast.tag first);
  (* [third] holds the smallest codepoint and was interned last, so a
     structural order would rank it first. It does not. *)
  check
    "compare is not structural"
    (Ast.compare third first > 0 && Ast.compare third second > 0);
  check
    "compare is zero exactly on equal nodes"
    (Ast.compare first first = 0 && Ast.equal first first && Ast.compare first second <> 0);
  (* [eps] is the canonical [Seq] of nothing, so both of these are
     traps for a caller matching on shape. *)
  check "is_seq holds of eps" (Ast.is_seq Ast.eps);
  check "seq_children of eps is the empty list" (Ast.seq_children Ast.eps = []);
  check "is_alt does not hold of eps" (not (Ast.is_alt Ast.eps));
  (* Every constructor taking a raw codepoint validates it through
     Ucharset, which the .mli now says. *)
  let raises f =
    match f 0xD800 with
    | () -> false
    | exception Invalid_argument _ -> true
  and raises_high f =
    match f 0x110000 with
    | () -> false
    | exception Invalid_argument _ -> true
  in
  List.iter
    (fun (name, f) ->
       check (name ^ " raises on a surrogate") (raises f);
       check (name ^ " raises above U+10FFFF") (raises_high f))
    [ ("Ast.singleton", fun cp -> ignore (Ast.singleton cp))
    ; ("Ast.range", fun cp -> ignore (Ast.range ~lo:cp ~hi:cp))
    ; ("singleton", fun cp -> ignore (singleton cp))
    ; ("range", fun cp -> ignore (range ~lo:cp ~hi:cp))
    ; ("not_singleton", fun cp -> ignore (not_singleton cp))
    ; ("not_range", fun cp -> ignore (not_range ~lo:cp ~hi:cp))
    ; ("chars_of_list", fun cp -> ignore (chars_of_list [ cp ]))
    ; ("chars_in_ranges", fun cp -> ignore (chars_in_ranges [ cp, cp ]))
    ; ("one_of ~singles", fun cp -> ignore (one_of ~singles:[ cp ] ()))
    ; ("one_of ~ranges", fun cp -> ignore (one_of ~ranges:[ cp, cp ] ()))
    ];
  (* The [_char] and [_uchar] forms take scalar values already. *)
  check "singleton_char cannot raise" (not (is_empty (singleton_char 'a')));
  check
    "singleton_uchar cannot raise"
    (not (is_empty (singleton_uchar (Uchar.of_int 0x3BB))))
;;

(* -- the language the engine computes -------------------------------------- *)

let () =
  let st = Random.State.make [| 20260829 |] in
  for _ = 1 to 3000 do
    let r, oracle = gen ~alphabet st 3 in
    let a = to_ast r in
    List.iter (fun s -> check "eval agrees with oracle" (Ast.eval a s = oracle s)) corpus
  done
;;

(* -- canonical form the smart constructors promise ------------------------- *)

let () =
  let st = Random.State.make [| 4242 |] in
  for _ = 1 to 20_000 do
    let r, _ = gen ~alphabet st 4 in
    let r = to_ast r in
    if Ast.is_alt r
    then (
      let xs = Ast.alt_children r in
      check
        "Alt sorted distinct"
        (List.map Ast.tag xs = List.map Ast.tag (Ast.sort_distinct xs));
      check "Alt length >= 2" (List.length xs >= 2);
      check "Alt no empty child" (not (List.exists Ast.is_empty xs));
      check "Alt no nested Alt" (not (List.exists Ast.is_alt xs));
      check "Alt at most one Chars" (List.length (List.filter Ast.is_chars xs) <= 1));
    if Ast.is_seq r
    then (
      let xs = Ast.seq_children r in
      (* [eps] is the canonical empty [Seq]; anything else has two or
         more children, the singleton having been unwrapped. *)
      check "Seq length 0 or >= 2" (xs = [] || List.length xs >= 2);
      check "Seq no eps child" (not (List.exists Ast.is_eps xs));
      check "Seq no empty child" (not (List.exists Ast.is_empty xs)))
  done
;;

(* -- the parser ------------------------------------------------------------ *)

let () =
  (* Every corpus string is a run of literals, so it parses and matches
     exactly itself. *)
  List.iter
    (fun s ->
       match of_string s with
       | Error e -> check ("of_string " ^ s ^ ": " ^ e.msg) false
       | Ok r ->
         let a = to_ast r in
         List.iter
           (fun t -> check "parsed literal matches only itself" (Ast.eval a t = (t = s)))
           corpus)
    corpus;
  (* Sources that must be rejected, with the offset the caret lands on. *)
  List.iter
    (fun (src, pos) ->
       match of_string src with
       | Ok _ -> check ("of_string should reject " ^ src) false
       | Error e -> check ("of_string " ^ src ^ " reports the right offset") (e.pos = pos))
    [ "a(", 1; "a)b", 1; "[a-", 0; "*a", 0; "[z-a]", 1; "\\q", 0; "[]", 0; "a\\", 1 ];
  (* Shapes the grammar has to get right. *)
  let same src t = check ("of_string " ^ src) (of_string src = Ok t) in
  let a = singleton_char 'a'
  and b = singleton_char 'b'
  and c = singleton_char 'c' in
  same "abc" (seqs [ a; b; c ]);
  same "a|bc" (alts [ a; seq b c ]);
  same "(a|b)c" (seq (alts [ a; b ]) c);
  same "a*" (star a);
  same "~a" (complement a);
  same "a&b" (inters [ a; b ]);
  (* The prefix/postfix interaction the published grammar names. A
     postfix binds tighter than [~], and [~] takes only the one repeat
     after it. These pin the .mli's grammar block to the parser: they
     are the shapes the two used to disagree on. *)
  same "~a*" (complement (star a));
  same "~a+" (complement (plus a));
  same "~a?" (complement (opt a));
  same "(~a)*" (star (complement a));
  same "~ab" (seq (complement a) b);
  same "~(ab)" (complement (seq a b));
  same "~~a*" (complement (complement (star a)));
  (* The reading is observable, not just structural: [a*] matches the
     empty string, so its complement does not. *)
  let matches src t =
    match of_string src with
    | Error e -> check (Printf.sprintf "of_string %S: %s" src e.msg) false
    | Ok r -> check (Printf.sprintf "%S matches %S" src t) (Ast.eval (to_ast r) t)
  in
  let rejects src t =
    match of_string src with
    | Error e -> check (Printf.sprintf "of_string %S: %s" src e.msg) false
    | Ok r ->
      check (Printf.sprintf "%S does not match %S" src t) (not (Ast.eval (to_ast r) t))
  in
  rejects "~a*" "";
  matches "(~a)*" "";
  matches "~a*" "b";
  rejects "~a*" "aa";
  same "[^a]" (not_singleton_char 'a');
  same "." any;
  (* The class helpers agree with the sources they stand for. *)
  same "[abc]" (chars_of_char_list [ 'a'; 'b'; 'c' ]);
  same "[abc]" (chars_of_list [ 0x61; 0x62; 0x63 ]);
  same "[abc]" (chars_of_uchar_list (List.map Uchar.of_int [ 0x61; 0x62; 0x63 ]));
  same "[a-cx-z]" (chars_in_char_ranges [ 'a', 'c'; 'x', 'z' ]);
  same "[a-cx-z]" (chars_in_ranges [ 0x61, 0x63; 0x78, 0x7A ]);
  same
    "[a-cx-z]"
    (chars_in_uchar_ranges
       [ Uchar.of_int 0x61, Uchar.of_int 0x63; Uchar.of_int 0x78, Uchar.of_int 0x7A ]);
  same "[a-c_]" (one_of_char ~singles:[ '_' ] ~ranges:[ 'a', 'c' ] ());
  same "[a-c_]" (one_of ~singles:[ 0x5F ] ~ranges:[ 0x61, 0x63 ] ());
  same "[a-c]" (one_of_char ~ranges:[ 'a', 'c' ] ());
  same "[ab]" (one_of_char ~singles:[ 'a'; 'b' ] ());
  check "one_of with nothing is the empty language" (is_empty (one_of ()))
;;

(* -- escapes --------------------------------------------------------------- *)

(* Whether a string is well formed UTF-8. An error message goes to a
   log or a terminal, so one carrying a raw byte out of the source is
   a defect in its own right. *)
let is_utf8 s =
  let ok = ref true
  and i = ref 0 in
  while !i < String.length s do
    let d = String.get_utf_8_uchar s !i in
    if not (Uchar.utf_decode_is_valid d) then ok := false;
    i := !i + Uchar.utf_decode_length d
  done;
  !ok
;;

let () =
  let rejects src =
    check
      (Printf.sprintf "of_string should reject %S" src)
      (match of_string src with
       | Ok _ -> false
       | Error _ -> true)
  in
  let parses_as src t =
    check (Printf.sprintf "of_string %S" src) (of_string src = Ok t)
  in
  let errors_with src msg =
    check
      (Printf.sprintf "of_string %S reports %S" src msg)
      (match of_string src with
       | Error e -> e.msg = msg
       | Ok _ -> false)
  in
  (* [int_of_string] reads [_] as a digit separator, so [\u{6_1}] and
     [\u{61_}] both parsed as [a]. *)
  rejects "\\u{6_1}";
  rejects "\\u{61_}";
  rejects "\\u{_61}";
  rejects "\\u{6 1}";
  rejects "\\u{6+1}";
  rejects "\\u{0x61}";
  (* Hex digits still parse, either case, leading zeros or not. *)
  parses_as "\\u{61}" (singleton 0x61);
  parses_as "\\u{0061}" (singleton 0x61);
  parses_as "\\u{3bb}" (singleton 0x3BB);
  parses_as "\\u{3BB}" (singleton 0x3BB);
  parses_as "\\u{10FFFF}" (singleton 0x10FFFF);
  (* Out of range, including a run too long for an int. The fold stops
     climbing rather than overflowing, so the message names the value
     rather than blaming the digits. *)
  errors_with "\\u{110000}" "\\u{110000} is not a Unicode scalar value";
  errors_with "\\u{D800}" "\\u{D800} is not a Unicode scalar value";
  errors_with
    "\\u{FFFFFFFFFFFFFFFF}"
    "\\u{FFFFFFFFFFFFFFFF} is not a Unicode scalar value";
  (* An unknown escape interpolated the byte after the backslash with
     [%c], so a multi-byte character gave a message that was not
     valid UTF-8. *)
  errors_with "\\\u{E9}" "unknown escape \\\u{E9}";
  errors_with "\\\u{3BB}" "unknown escape \\\u{3BB}";
  errors_with "\\\u{10400}" "unknown escape \\\u{10400}";
  errors_with "\\q" "unknown escape \\q";
  (* A byte that is not UTF-8 at all is refused as such, and the caret
     lands on the byte rather than on the backslash. *)
  errors_with "\\\xff" "malformed UTF-8";
  check
    "the caret lands on the malformed byte"
    (match of_string "\\\xff" with
     | Error e -> e.pos = 1
     | Ok _ -> false);
  (* The property behind those two: no message carries a raw byte out
     of the source. Run over every character the round-trip alphabet
     covers, which is where the multi-byte ones are. *)
  let bad = ref 0 in
  Array.iter
    (fun cp ->
       let b = Buffer.create 8 in
       Buffer.add_char b '\\';
       Buffer.add_utf_8_uchar b (Uchar.of_int cp);
       match of_string (Buffer.contents b) with
       | Ok _ -> ()
       | Error e -> if not (is_utf8 e.msg) then incr bad)
    meta_alphabet;
  check "every escape error message is valid UTF-8" (!bad = 0);
  (* Every ASCII byte, either side of the rule the .mli states: a
     backslash before a printable non-alphanumeric is that character,
     and a backslash before space, a C0 control or DEL is an error.
     The letters and digits are left out, being the named escapes and
     the errors the block above covers. *)
  let escaped cp =
    let b = Buffer.create 4 in
    Buffer.add_char b '\\';
    Buffer.add_char b (Char.chr cp);
    Buffer.contents b
  in
  let is_alnum cp =
    (cp >= Char.code '0' && cp <= Char.code '9')
    || (cp >= Char.code 'a' && cp <= Char.code 'z')
    || (cp >= Char.code 'A' && cp <= Char.code 'Z')
  in
  let punct = ref 0
  and refused = ref 0 in
  for cp = 0x21 to 0x7E do
    if not (is_alnum cp)
    then (
      incr punct;
      check
        (Printf.sprintf "escaped U+%04X is that character" cp)
        (of_string (escaped cp) = Ok (singleton cp)))
  done;
  List.iter
    (fun cp ->
       incr refused;
       check
         (Printf.sprintf "escaped U+%04X is refused" cp)
         (match of_string (escaped cp) with
          | Error e -> e.msg = Printf.sprintf "unknown escape: U+%04X" cp
          | Ok _ -> false))
    (List.init 0x21 (fun cp -> cp) @ [ 0x7F ]);
  check
    (Printf.sprintf
       "the ASCII sweep covers both sides (%d escapable, %d refused)"
       !punct
       !refused)
    (!punct = 32 && !refused = 34);
  (* And no message carries a control character either, which is the
     same defect as the raw byte above wearing different clothes. *)
  let ctrl = ref 0 in
  for cp = 0x00 to 0x7F do
    match of_string (escaped cp) with
    | Ok _ -> ()
    | Error e ->
      if String.exists (fun ch -> Char.code ch < 0x20 || Char.code ch = 0x7F) e.msg
      then incr ctrl
  done;
  check "no escape error message carries a control character" (!ctrl = 0)
;;

(* -- source round trips ---------------------------------------------------- *)

(* Equality of the lowered terms, which is what [Regex.equivalent]
   used to be. The round trips below use it rather than [equivalent],
   which now decides the language and would pass a printer that
   rendered [a*] as ["a*a*"]. *)
let same_form a b = Ast.equal (to_ast a) (to_ast b)

let round_trips ~alphabet ~seed ~n =
  let st = Random.State.make [| seed |] in
  for _ = 1 to n do
    let r, _ = gen ~alphabet st 3 in
    (* Rendering and parsing check each other, so a precedence slip on
       either side shows up here. *)
    let src = to_string r in
    (match of_string src with
     | Error e ->
       check
         (Printf.sprintf "to_string produced unparseable source %S: %s" src e.msg)
         false
     | Ok back -> check (Printf.sprintf "round trip %S" src) (same_form r back));
    (* Oniguruma output is a subset of the same syntax, once [(?:] is
       read as a group. *)
    match to_oniguruma r with
    | Error _ -> ()
    | Ok oni ->
      (match of_string oni with
       | Error e -> check (Printf.sprintf "oniguruma %S unparseable: %s" oni e.msg) false
       | Ok back ->
         check (Printf.sprintf "oniguruma round trip %S" oni) (same_form r back))
  done
;;

let () =
  round_trips ~alphabet ~seed:7 ~n:4000;
  (* The same round trip over every character the grammar reserves. An
     escape the emitter forgets makes the output reparse as an operator,
     which no alphabet of plain letters can show. *)
  round_trips ~alphabet:meta_alphabet ~seed:1109 ~n:4000
;;

(* The reserved characters, one at a time, as literals. [&] is the case
   that used to come back silently as the empty language rather than as
   an error. *)
let () =
  Array.iter
    (fun cp ->
       let r = singleton cp in
       match to_oniguruma r with
       | Error _ -> ()
       | Ok oni ->
         (match of_string oni with
          | Error e ->
            check
              (Printf.sprintf "literal U+%04X emits unparseable %S: %s" cp oni e.msg)
              false
          | Ok back ->
            check
              (Printf.sprintf "literal U+%04X survives oniguruma as %S" cp oni)
              (same_form r back)))
    meta_alphabet;
  (* The exact reproductions from the review. *)
  List.iter
    (fun src ->
       match of_string src with
       | Error e -> check (Printf.sprintf "of_string %S: %s" src e.msg) false
       | Ok r ->
         (match to_oniguruma r with
          | Error _ -> check (Printf.sprintf "to_oniguruma %S refused" src) false
          | Ok oni ->
            (match of_string oni with
             | Error e ->
               check (Printf.sprintf "%S -> oniguruma %S: %s" src oni e.msg) false
             | Ok back ->
               check
                 (Printf.sprintf "%S -> oniguruma %S round trips" src oni)
                 (same_form r back))))
    [ "\\&"; "a\\&b"; "\\~"; "\\~*"; "a\\&b|\\~" ]
;;

(* -- the printers over the whole public type ------------------------------- *)

(* [=] on a [Regex.t] compares the [Ucharset.t] payloads structurally.
   Set equality is what is wanted here, so go through [Ucharset.equal]
   rather than trusting the two to agree. *)
let rec same_regex a b =
  match a, b with
  | Chars x, Chars y | Neg_chars x, Neg_chars y -> Ucharset.equal x y
  | Eps, Eps -> true
  | Seq xs, Seq ys | Alt xs, Alt ys | Inter xs, Inter ys ->
    List.length xs = List.length ys && List.for_all2 same_regex xs ys
  | Star x, Star y | Plus x, Plus y | Opt x, Opt y | Complement x, Complement y ->
    same_regex x y
  | _ -> false
;;

(* [pp] breaks at the formatter's margin, so widen it: this compares
   renderings, not layouts. *)
let pp_string t =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf 1_000_000;
  Format.fprintf ppf "%a" pp t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
;;

(* [pp] is a debug view, so its one job is to say which node you are
   holding. That is injectivity: two terms sharing a rendering means
   the rendering does not identify either. Both of the review's cases
   are collisions of exactly that shape --- [Neg_chars c] against
   [Complement (Chars c)], and [Star (Neg_chars c)] against
   [Complement (Star (Chars c))], the second because the prefix [~] was
   printed at atom precedence and so was never parenthesised. *)
let pp_injective ~alphabet ~seed ~n =
  let st = Random.State.make [| seed |] in
  let seen = Hashtbl.create 4096 in
  let collisions = ref 0 in
  for _ = 1 to n do
    let r, _ = gen ~alphabet st 3 in
    let rendered = pp_string r in
    match Hashtbl.find_opt seen rendered with
    | Some prev ->
      if not (same_regex prev r)
      then (
        if !collisions = 0 then Printf.printf "  first pp collision: %s\n" rendered;
        incr collisions)
    | None -> Hashtbl.add seen rendered r
  done;
  check
    (Printf.sprintf
       "pp is injective (seed %d: %d renderings, %d collisions)"
       seed
       (Hashtbl.length seen)
       !collisions)
    (!collisions = 0)
;;

let () =
  pp_injective ~alphabet ~seed:31337 ~n:20_000;
  pp_injective ~alphabet:meta_alphabet ~seed:90210 ~n:20_000;
  (* The review's reproductions, named. *)
  let a = Ucharset.singleton_char 'a' in
  check
    "pp tells a negated class from a complemented one"
    (pp_string (Neg_chars a) <> pp_string (Complement (Chars a)));
  check
    "pp parenthesises a starred negated class"
    (pp_string (Star (Neg_chars a)) <> pp_string (Complement (Star (Chars a))));
  (* Empty [Alt] and empty [Inter] printed as nothing, which is what a
     [Seq] of nothing prints and is the language of [eps], not theirs. *)
  check "pp tells the empty language from eps" (pp_string (Alt []) <> pp_string Eps);
  check
    "pp tells the empty language from an empty Seq"
    (pp_string (Alt []) <> pp_string (Seq []));
  check "pp tells an empty Inter from eps" (pp_string (Inter []) <> pp_string Eps);
  check "pp tells the two empty lists apart" (pp_string (Alt []) <> pp_string (Inter []))
;;

(* [to_string] is documented as the source [of_string] reads back, and
   the constructors are public, so it has to be total over the type.
   [Alt \[\]] is the empty language and [Inter \[\]] is every string;
   both used to render as the empty source, which reads back as
   [eps]. *)
let () =
  let a = Chars (Ucharset.singleton_char 'a') in
  let reads_back name t =
    let src = to_string t in
    match of_string src with
    | Error e ->
      check (Printf.sprintf "%s: to_string gave unparseable %S: %s" name src e.msg) false
    | Ok back ->
      check (Printf.sprintf "%s reads back from %S" name src) (same_form t back)
  in
  List.iter
    (fun (name, t) -> reads_back name t)
    [ "Alt []", Alt []
    ; "Inter []", Inter []
    ; "Star (Alt [])", Star (Alt [])
    ; "Star (Inter [])", Star (Inter [])
    ; "Complement (Alt [])", Complement (Alt [])
    ; "Seq [Alt []; a]", Seq [ Alt []; a ]
    ; "Seq [Inter []; a]", Seq [ Inter []; a ]
    ; "Alt [Inter []; a]", Alt [ Inter []; a ]
    ; "Inter [Inter []; a]", Inter [ Inter []; a ]
    ; "Alt [Alt []; a]", Alt [ Alt []; a ]
    ];
  (* [same_form] would be satisfied by any two terms that agree, so
     pin the languages the two empty lists denote as well. *)
  check "Alt [] is the empty language" (not (Ast.eval (to_ast (Alt [])) ""));
  check "Alt [] matches nothing at all" (not (Ast.eval (to_ast (Alt [])) "a"));
  check "Inter [] takes the empty string" (Ast.eval (to_ast (Inter [])) "");
  check "Inter [] takes any string" (Ast.eval (to_ast (Inter [])) "abc")
;;

(* -- str rejects the bytes of_string rejects ------------------------------- *)

(* [String.get_utf_8_uchar] answers U+FFFD on a bad byte instead of
   failing, so [str] used to build a term the caller did not write,
   silently, over input the parser refuses. *)
let () =
  List.iter
    (fun s ->
       check
         (Printf.sprintf "of_string rejects %S" s)
         (match of_string s with
          | Error e -> e.msg = "malformed UTF-8"
          | Ok _ -> false);
       check
         (Printf.sprintf "Regex.str rejects %S" s)
         (match str s with
          | exception Invalid_argument _ -> true
          | _ -> false);
       check
         (Printf.sprintf "Ast.str rejects %S" s)
         (match Ast.str s with
          | exception Invalid_argument _ -> true
          | _ -> false))
    [ "\xff\xfe" (* not UTF-8 at all *)
    ; "\xc3" (* truncated two byte sequence *)
    ; "\xe0\xa4" (* truncated three byte sequence *)
    ; "\x80" (* a lone continuation byte *)
    ; "\xed\xa0\x80" (* a surrogate, ill formed in UTF-8 *)
    ; "\xf4\x90\x80\x80" (* above U+10FFFF *)
    ];
  (* Only the malformed input is refused. A well formed U+FFFD is a
     codepoint like any other and still goes through. *)
  let fffd = "\xef\xbf\xbd" in
  check "Regex.str keeps a real U+FFFD" (Ast.eval (to_ast (str fffd)) fffd);
  check "Ast.str keeps a real U+FFFD" (Ast.eval (Ast.str fffd) fffd);
  check
    "Regex.str still takes valid text"
    (Ast.eval (to_ast (str "\xce\xbbx")) "\xce\xbbx");
  check "Ast.str still takes valid text" (Ast.eval (Ast.str "\xce\xbbx") "\xce\xbbx");
  check "str of the empty string is eps" (is_eps (str ""))
;;

(* -- the first-set guard never rejects a live codepoint --------------------- *)

(* [deriv] answers [empty] for every codepoint outside [first_set], so an
   under-approximation there — or bounds that disagree with the set they
   summarise — is a wrong answer with no error attached. The oracle
   shares no code with the engine, so this pins the guard from outside:
   every string the oracle matches has to begin with a codepoint the
   first set admits, and the derivative on that codepoint has to be
   live. *)
let () =
  let st = Random.State.make [| 8675309 |] in
  for _ = 1 to 2000 do
    let r, oracle = gen ~alphabet st 3 in
    let a = to_ast r in
    let fs = Ast.first_set a in
    List.iter
      (fun s ->
         if String.length s > 0 && oracle s
         then (
           let cp = Char.code s.[0] in
           check
             (Printf.sprintf "first_set admits the start of the match %S" s)
             (Ucharset.mem fs cp);
           check
             (Printf.sprintf "deriv stays live on the start of the match %S" s)
             (not (Ast.is_empty (Ast.deriv a ~uchr:cp)))))
      corpus
  done
;;

(* -- codepoints beyond ASCII ----------------------------------------------- *)

let () =
  let matches r s = Ast.eval (to_ast r) s in
  let lam = "\xce\xbb" in
  (* [str] decodes UTF-8, so a two byte character is one codepoint. *)
  check "str matches its own text" (matches (str (lam ^ "x")) (lam ^ "x"));
  check "str is not byte oriented" (not (matches (str (lam ^ "x")) "x"));
  check "any is one codepoint" (matches any lam);
  check "any is not one byte" (not (matches (seq any any) lam));
  (* Escapes and classes carry codepoints, not bytes. *)
  (match of_string "\\u{3BB}" with
   | Error _ -> check "parse \\u{3BB}" false
   | Ok r ->
     check "\\u escape matches the character" (matches r lam);
     check "\\u escape matches nothing else" (not (matches r "a")));
  check "negated class excludes the character" (not (matches (not_singleton 0x3BB) lam));
  check "negated class admits others" (matches (not_singleton 0x3BB) "a");
  (* Supplementary plane, four bytes. *)
  let deseret = "\xf0\x90\x90\x80" in
  check "supplementary codepoint" (matches (singleton 0x10400) deseret);
  check "supplementary in a range" (matches (range ~lo:0x10000 ~hi:0x10FFFF) deseret);
  check
    "round trips through source"
    (match of_string (to_string (singleton 0x10400)) with
     | Ok r -> matches r deseret
     | Error _ -> false)
;;

(* -- the DFA against the regexes and against a reference minimisation ------ *)

module Dfa = Redfa.Dfa

(* A DFA alphabet that leaves the BMP. The oracle in [gen] compares
   bytes, so it cannot run this; everything below compares the
   automaton against [Ast.eval] on the same term instead, which is the
   reference the DFA tests have always used. The surrogate boundaries
   and a supplementary-plane codepoint are here because the DFA path
   had never been driven outside the BMP: [dfa_accepts] used to index
   bytes. *)
let wide_alphabet = [| 0x00; Char.code '~'; 0x3BB; 0xD7FF; 0xE000; 0x10400 |]

(* Every string of up to [len] codepoints over [alphabet], as
   codepoint lists. Strings, being UTF-8, are not what the automaton
   consumes. *)
let cp_corpus ~alphabet ~len =
  let rec grow acc k =
    if k = 0
    then acc
    else (
      let longer =
        List.concat_map
          (fun s -> Array.to_list (Array.map (fun c -> s @ [ c ]) alphabet))
          acc
      in
      acc @ grow longer (k - 1))
  in
  grow [ [] ] len
;;

let utf8 cps =
  let b = Buffer.create 8 in
  List.iter (fun cp -> Buffer.add_utf_8_uchar b (Uchar.of_int cp)) cps;
  Buffer.contents b
;;

(* Traverse the DFA over a codepoint sequence, returning the accepts
   list at the state it lands in, or [] if it gets stuck. Stuck means
   every item's derivative died, so no token can match, matching the
   empty accepts list the reference gives. *)
let dfa_accepts dfa cps =
  let rec go id = function
    | [] -> Dfa.accepts dfa id
    | cp :: rest ->
      (match
         List.find_opt (fun (cs, _) -> Ucharset.mem cs cp) (Dfa.transitions dfa id)
       with
       | None -> []
       | Some (_, dst) -> go dst rest)
  in
  go (Dfa.initial dfa) cps
;;

let same_dfa a b =
  Dfa.num_states a = Dfa.num_states b
  &&
  let ok = ref true in
  Dfa.iter_states a (fun id ->
    if Dfa.accepts a id <> Dfa.accepts b id then ok := false;
    if Dfa.reaches a id <> Dfa.reaches b id then ok := false;
    let ta = Dfa.transitions a id
    and tb = Dfa.transitions b id in
    if List.length ta <> List.length tb
    then ok := false
    else
      List.iter2
        (fun (c1, d1) (c2, d2) ->
           if not (Ucharset.equal c1 c2 && d1 = d2) then ok := false)
        ta
        tb);
  !ok
;;

(* -- a reference minimisation ---------------------------------------------- *)

(* Myhill-Nerode by Moore's table-filling algorithm on the completed
   automaton, over a finite alphabet. It shares no code with
   [Dfa.minimise]: a different algorithm (mark distinguishable pairs
   to a fixpoint, rather than refine a partition) over a different
   representation (a dense transition matrix on representative
   codepoints, rather than charset signatures).

   Two codepoints in one block of the common refinement of every
   state's transition labels behave alike from every state, so one
   representative per block is a faithful finite alphabet. *)
let effective_alphabet (d : Dfa.t) =
  let parts = ref [] in
  Dfa.iter_states d (fun id ->
    let labels = List.map fst (Dfa.transitions d id) in
    (* The completion: what the state has no transition on. *)
    let rest = Ucharset.comp (Ucharset.union_list labels) in
    parts := Ucharset.Partition.of_blocks (rest :: labels) :: !parts);
  Array.of_list (Ucharset.Partition.representatives (Ucharset.Partition.meet_all !parts))
;;

type reference =
  { min_states : int (* states of the minimal partial DFA *)
  ; dead : bool array (* per original state: empty residual language *)
  }

let reference_min (d : Dfa.t) =
  let n = Dfa.num_states d in
  let sink = n in
  let m = n + 1 in
  let sigma = effective_alphabet d in
  let k = Array.length sigma in
  let delta = Array.make_matrix m k sink in
  for id = 0 to n - 1 do
    let ts = Dfa.transitions d id in
    for a = 0 to k - 1 do
      delta.(id).(a)
      <- (match List.find_opt (fun (cs, _) -> Ucharset.mem cs sigma.(a)) ts with
          | Some (_, dst) -> dst
          | None -> sink)
    done
  done;
  let acc id = if id = sink then [] else Dfa.accepts d id in
  let dist = Array.make_matrix m m false in
  for p = 0 to m - 1 do
    for q = 0 to m - 1 do
      if acc p <> acc q then dist.(p).(q) <- true
    done
  done;
  let changed = ref true in
  while !changed do
    changed := false;
    for p = 0 to m - 1 do
      for q = p + 1 to m - 1 do
        if not dist.(p).(q)
        then
          for a = 0 to k - 1 do
            if (not dist.(p).(q)) && dist.(delta.(p).(a)).(delta.(q).(a))
            then (
              dist.(p).(q) <- true;
              dist.(q).(p) <- true;
              changed := true)
          done
      done
    done
  done;
  (* One class per state, named by its least member. *)
  let repr = Array.make m (-1) in
  for p = 0 to m - 1 do
    if repr.(p) = -1
    then
      for q = p to m - 1 do
        if (not dist.(p).(q)) && repr.(q) = -1 then repr.(q) <- p
      done
  done;
  let sink_class = repr.(sink) in
  let live = Hashtbl.create 16 in
  let rec walk id =
    if repr.(id) <> sink_class && not (Hashtbl.mem live repr.(id))
    then (
      Hashtbl.add live repr.(id) ();
      for a = 0 to k - 1 do
        walk delta.(id).(a)
      done)
  in
  walk 0;
  let count = Hashtbl.length live in
  { min_states = (if count = 0 then 1 else count)
  ; dead = Array.init n (fun id -> repr.(id) = sink_class)
  }
;;

(* Which state of [m] stands for each state of [d]. Every state of [d]
   is reachable, so walking the two together from their initial states
   reaches all of them; [-1] marks one [minimise] dropped, which it
   may do only for a state with an empty residual language. *)
let correspondence (d : Dfa.t) (m : Dfa.t) =
  let map = Array.make (Dfa.num_states d) (-1) in
  let rec walk o s =
    if map.(o) = -1
    then (
      map.(o) <- s;
      List.iter
        (fun (cs, o') ->
           match Ucharset.min_elt_opt cs with
           | None -> ()
           | Some cp ->
             (match
                List.find_opt (fun (cs', _) -> Ucharset.mem cs' cp) (Dfa.transitions m s)
              with
              | Some (_, s') -> walk o' s'
              | None -> ()))
        (Dfa.transitions d o))
  in
  walk (Dfa.initial d) (Dfa.initial m);
  map
;;

(* -- the properties -------------------------------------------------------- *)

(* Case ids some reachable state accepts: what [reaches] would name if
   it were exact. A fixpoint over the transition graph, which shares
   nothing with the item-set projection [reaches] comes from. *)
let matchable (d : Dfa.t) =
  let n = Dfa.num_states d in
  let out = Array.make n [] in
  let changed = ref true in
  while !changed do
    changed := false;
    for id = n - 1 downto 0 do
      let acc =
        List.sort_uniq
          Int.compare
          (List.fold_left
             (fun a (_, dst) -> out.(dst) @ a)
             (Dfa.accepts d id)
             (Dfa.transitions d id))
      in
      if acc <> out.(id)
      then (
        out.(id) <- acc;
        changed := true)
    done
  done;
  out
;;

let check_dfa ~label ~alphabet ~len ~seed ~trials ~depth ~max_tokens =
  let st = Random.State.make [| seed |] in
  let corp = cp_corpus ~alphabet ~len in
  let over_built = ref 0
  and over_min = ref 0 in
  for _ = 1 to trials do
    let tokens =
      List.init
        (1 + Random.State.int st max_tokens)
        (fun i -> i, fst (gen ~alphabet st depth))
    in
    let dfa = Dfa.of_tokens tokens in
    let mini = Dfa.minimise dfa in
    let name what = Printf.sprintf "%s: %s" label what in
    (* The language, against the regexes themselves. *)
    List.iter
      (fun cps ->
         let s = utf8 cps in
         let expected =
           List.filter_map
             (fun (cid, r) -> if Ast.eval (to_ast r) s then Some cid else None)
             tokens
         in
         check (name "dfa accepts") (dfa_accepts dfa cps = expected);
         check (name "minimised dfa accepts") (dfa_accepts mini cps = expected))
      corp;
    (* The size, against a reference Myhill-Nerode minimisation. The
       refinement used to sign a state by its stored transitions, which
       tells "no transition on c" apart from "a transition on c into a
       dead state", so dead states and their in-edges survived: about
       one random rule set in ten came out above the minimum. *)
    let reference = reference_min dfa in
    check
      (name "minimise reaches the minimum")
      (Dfa.num_states mini = reference.min_states);
    check (name "minimise is idempotent") (same_dfa mini (Dfa.minimise mini));
    (* Nothing dead survives, the one exception being the automaton
       that is nothing but a dead state. *)
    let dead_survivors = ref 0 in
    Dfa.iter_states mini (fun id -> if Dfa.is_dead mini id then incr dead_survivors);
    check
      (name "no dead state survives")
      (!dead_survivors = 0 || (Dfa.num_states mini = 1 && reference.dead.(0)));
    (* What both automata owe their callers. [minimise] rebuilds every
       transition list, dropping the edges into dead states, so it has
       to be held to the same promises the construction is. *)
    let well_formed what a =
      Dfa.iter_states a (fun id ->
        let acc = Dfa.accepts a id
        and rch = Dfa.reaches a id in
        check
          (name (what ^ " reaches covers accepts"))
          (List.for_all (fun c -> List.mem c rch) acc);
        check
          (name (what ^ " reaches ascending and distinct"))
          (rch = List.sort_uniq Int.compare rch);
        check
          (name (what ^ " accepts ascending and distinct"))
          (acc = List.sort_uniq Int.compare acc);
        check
          (name (what ^ " is_dead agrees with accepts and transitions"))
          (Dfa.is_dead a id = (acc = [] && Dfa.transitions a id = []));
        (* Transitions out of a state are pairwise disjoint, which is
           what makes the traversal above deterministic, and they
           arrive in ascending order of least codepoint. *)
        let css = List.map fst (Dfa.transitions a id) in
        let rec disjoint = function
          | [] | [ _ ] -> true
          | x :: rest ->
            List.for_all (fun y -> Ucharset.is_empty (Ucharset.inter x y)) rest
            && disjoint rest
        in
        check (name (what ^ " transitions disjoint")) (disjoint css);
        check
          (name (what ^ " no empty transition label"))
          (not (List.exists Ucharset.is_empty css));
        let mins = List.filter_map Ucharset.min_elt_opt css in
        check (name (what ^ " labels ascending")) (mins = List.sort Int.compare mins))
    in
    (* [reaches] is documented as an over-approximation of what can
       still match, and as one that survives [minimise]. Both halves
       are pinned: never short of what is matchable, and sometimes
       longer, on each automaton. *)
    List.iter
      (fun (what, a, counter) ->
         let m = matchable a in
         Dfa.iter_states a (fun id ->
           let r = Dfa.reaches a id in
           check
             (name (what ^ " reaches covers what can still match"))
             (List.for_all (fun c -> List.mem c r) m.(id));
           if r <> m.(id) then incr counter))
      [ "built", dfa, over_built; "minimised", mini, over_min ];
    well_formed "built" dfa;
    well_formed "minimised" mini;
    (* [reaches] at a merged state is the union of those merged: the
       documented claim, checked against the correspondence between
       the two automata rather than assumed. *)
    if reference.dead.(0)
    then (
      check (name "an empty language minimises to one state") (Dfa.num_states mini = 1);
      check (name "that state is dead") (Dfa.is_dead mini 0);
      let all = ref [] in
      Dfa.iter_states dfa (fun id -> all := Dfa.reaches dfa id @ !all);
      check
        (name "and carries the union of every reaches")
        (Dfa.reaches mini 0 = List.sort_uniq Int.compare !all))
    else (
      let map = correspondence dfa mini in
      let merged = Array.make (Dfa.num_states mini) [] in
      Dfa.iter_states dfa (fun id ->
        check
          (name "a state is dropped exactly when it is dead")
          (map.(id) = -1 = reference.dead.(id));
        if map.(id) <> -1
        then (
          check
            (name "a merged state accepts what it stood for")
            (Dfa.accepts mini map.(id) = Dfa.accepts dfa id);
          merged.(map.(id)) <- Dfa.reaches dfa id @ merged.(map.(id))));
      Dfa.iter_states mini (fun id ->
        check
          (name "reaches at a merged state is the union of those merged")
          (Dfa.reaches mini id = List.sort_uniq Int.compare merged.(id))))
  done;
  check
    (Printf.sprintf
       "%s: reaches over-approximates, before and after minimise (%d, %d states)"
       label
       !over_built
       !over_min)
    (!over_built > 0 && !over_min > 0)
;;

let () =
  (* The single-byte alphabet the suite has always used, over every
     string of up to three characters. *)
  check_dfa ~label:"ascii" ~alphabet ~len:3 ~seed:99 ~trials:400 ~depth:3 ~max_tokens:4;
  check_dfa
    ~label:"ascii deep"
    ~alphabet
    ~len:3
    ~seed:20260902
    ~trials:150
    ~depth:5
    ~max_tokens:3;
  (* The same, outside the BMP and across the surrogate boundaries. *)
  check_dfa
    ~label:"wide"
    ~alphabet:wide_alphabet
    ~len:3
    ~seed:4711
    ~trials:150
    ~depth:3
    ~max_tokens:3
;;

(* The review's own reproduction. In "a(b.*&c.*)" the parenthesised
   half is the empty language said in a way the normal form does not
   notice, so deriving [a] leaves a state that accepts nothing and
   goes nowhere. It used to survive minimisation, along with the edge
   into it. (The regex is quoted because a comment cannot hold a bare
   "*" followed by ")".) *)
let () =
  let d =
    Dfa.of_tokens
      [ ( 0
        , seq
            (singleton_char 'a')
            (inter
               (seq (singleton_char 'b') (star any))
               (seq (singleton_char 'c') (star any))) )
      ; 1, singleton_char 'd'
      ]
  in
  let m = Dfa.minimise d in
  check "a(b.*&c.*)|d builds three states" (Dfa.num_states d = 3);
  check "a(b.*&c.*)|d minimises to two" (Dfa.num_states m = 2);
  check
    "the edge into the dead state goes with it"
    (List.map (fun (cs, dst) -> Ucharset.to_list cs, dst) (Dfa.transitions m 0)
     = [ [ Char.code 'd', Char.code 'd' ], 1 ]);
  check "and the survivor is the one that accepts" (Dfa.accepts m 1 = [ 1 ]);
  (* A whole language that is empty keeps its one state, since an
     automaton has to have an initial one. *)
  let e =
    Dfa.minimise
      (Dfa.of_tokens [ 0, inter (singleton_char 'a') (complement (singleton_char 'a')) ])
  in
  check "an empty language is one dead state" (Dfa.num_states e = 1 && Dfa.is_dead e 0)
;;

(* -- deciding a language --------------------------------------------------- *)

(* The same decision, taken on the automaton. Both terms go in as
   tokens of one DFA, whose states are the pairs of residuals reached
   together, so a state accepting one and not the other is a
   separating string. Shares [deriv] with [Ast.equivalent], as
   everything here does, but nothing of how it decides. *)
let dfa_equivalent r1 r2 =
  let d = Dfa.of_tokens [ 0, r1; 1, r2 ] in
  let agree = ref true in
  Dfa.iter_states d (fun id ->
    match Dfa.accepts d id with
    | [ 0 ] | [ 1 ] -> agree := false
    | _ -> ());
  !agree
;;

(* Emptiness, likewise. [minimise] is documented to leave the empty
   language as one dead state, and no dead state otherwise. *)
let dfa_empty_language r =
  let m = Dfa.minimise (Dfa.of_tokens [ 0, r ]) in
  Dfa.num_states m = 1 && Dfa.is_dead m 0
;;

(* Rewritings that change the term and not the language. Each is an
   identity over the boolean operations, which no amount of
   flattening, sorting or dropping duplicates reaches: [Ast.equal]
   cannot see [(r&x) | (r&~x)] as [r]. [x] is a second random term, so
   the two sides have differently shaped automata. *)
let rewrite st r x =
  match Random.State.int st 5 with
  (* r & (every string) *)
  | 0 -> inter r (star any)
  (* r | (r & ~r) *)
  | 1 -> alt r (inter r (complement r))
  (* r & (r | ~r) *)
  | 2 -> inter r (alt r (complement r))
  (* (r & x) | (r & ~x) *)
  | 3 -> alt (inter r x) (inter r (complement x))
  (* r | (r & x) *)
  | _ -> alt r (inter r x)
;;

(* Random pairs, against the reference decision on the automaton and
   against the corpus. [len] and [alphabet] fix the corpus; a corpus
   disagreement is a witness string, so it settles the answer on its
   own and is the check that shares no decision procedure at all with
   the traversal. *)
let check_equivalence ~label ~alphabet ~len ~seed ~trials ~depth =
  let st = Random.State.make [| seed |] in
  let corp = List.map utf8 (cp_corpus ~alphabet ~len) in
  let name what = Printf.sprintf "%s: %s" label what in
  (* Non-vacuity: how many rewritten pairs the normal form still sees
     as different terms, and how many random pairs land either way. *)
  let rewritten_distinct = ref 0
  and random_same = ref 0
  and random_differ = ref 0 in
  for _ = 1 to trials do
    let r, _ = gen ~alphabet st depth in
    let x, _ = gen ~alphabet st depth in
    let s, _ = gen ~alphabet st depth in
    let ar = to_ast r
    and as_ = to_ast s in
    (* An identity the normal form cannot see. *)
    let r' = rewrite st r x in
    if not (same_form r r') then incr rewritten_distinct;
    check (name "a rewriting that preserves the language is equivalent") (equivalent r r');
    check (name "and the automaton agrees") (dfa_equivalent r r');
    (* Two independent terms, against the automaton. *)
    let got = equivalent r s in
    if got then incr random_same else incr random_differ;
    check (name "equivalent agrees with the automaton") (got = dfa_equivalent r s);
    check (name "equivalent is symmetric") (got = equivalent s r);
    check (name "equivalent is reflexive") (equivalent r r);
    (* A witness in the corpus settles it without any automaton. *)
    let witness = List.exists (fun w -> Ast.eval ar w <> Ast.eval as_ w) corp in
    if witness then check (name "a witness string means not equivalent") (not got);
    if got
    then check (name "equivalent terms agree on every string of the corpus") (not witness);
    (* Emptiness, the same three ways. *)
    let empty_r = is_empty_language r in
    check
      (name "is_empty_language agrees with the automaton")
      (empty_r = dfa_empty_language r);
    check
      (name "is_empty_language is equivalence to the empty language")
      (empty_r = equivalent r empty);
    if empty_r
    then
      check
        (name "an empty language matches nothing in the corpus")
        (not (List.exists (fun w -> Ast.eval ar w) corp));
    if List.exists (fun w -> Ast.eval ar w) corp
    then
      check (name "a match in the corpus means the language is not empty") (not empty_r)
  done;
  check
    (Printf.sprintf
       "%s: the rewritings are not vacuous (%d/%d still distinct terms)"
       label
       !rewritten_distinct
       trials)
    (!rewritten_distinct > trials / 2);
  check
    (Printf.sprintf
       "%s: random pairs land both ways (%d equivalent, %d not)"
       label
       !random_same
       !random_differ)
    (!random_same > 0 && !random_differ > 0)
;;

let () =
  check_equivalence ~label:"equiv ascii" ~alphabet ~len:3 ~seed:5150 ~trials:600 ~depth:3;
  check_equivalence
    ~label:"equiv ascii deep"
    ~alphabet
    ~len:3
    ~seed:60613
    ~trials:200
    ~depth:5;
  (* Outside the BMP and across the surrogate boundaries, where the
     two terms' partitions are refined together rather than shared. *)
  check_equivalence
    ~label:"equiv wide"
    ~alphabet:wide_alphabet
    ~len:2
    ~seed:8128
    ~trials:200
    ~depth:3
;;

(* The review's own examples, and the two questions the type could not
   answer before. Each names the structural test as well, so a
   rewriting of [equivalent] back into [same_form] fails here rather
   than passing quietly. *)
let () =
  let p src =
    match of_string src with
    | Ok r -> r
    | Error e -> failwith (Printf.sprintf "%S: %s" src e.msg)
  in
  let equiv src1 src2 =
    let a = p src1
    and b = p src2 in
    check
      (Printf.sprintf "%S is equivalent to %S" src1 src2)
      (equivalent a b && equivalent b a)
  in
  (* The same, where the normal form does not already see it, so the
     check cannot pass by lowering the two to one node. *)
  let equiv_beyond_aci src1 src2 =
    equiv src1 src2;
    check
      (Printf.sprintf "%S and %S are different terms" src1 src2)
      (not (same_form (p src1) (p src2)))
  in
  let differs src1 src2 =
    let a = p src1
    and b = p src2 in
    check
      (Printf.sprintf "%S is not equivalent to %S" src1 src2)
      ((not (equivalent a b)) && not (equivalent b a))
  in
  (* The two the review names. *)
  equiv_beyond_aci "a*a*" "a*";
  equiv_beyond_aci "(ab)*a" "a(ba)*";
  (* Identities over the boolean operations, which is what the
     library is for. *)
  equiv_beyond_aci "~(a|b)" "~a&~b";
  equiv_beyond_aci "~(a&b)" "~a|~b";
  equiv_beyond_aci "(a|b)*" "(a*b*)*";
  equiv_beyond_aci "a&~a" "[^\\u{0}-\\u{10FFFF}]";
  equiv_beyond_aci "\\u{3BB}*\\u{3BB}*" "\\u{3BB}*";
  (* Two the normal form does see, kept because they are the shapes a
     reader expects here: merging the classes of an [Alt] is exactly
     what it is for. *)
  equiv ".*" "(a|[^a])*";
  equiv "a|b" "b|a";
  (* Near misses, where a string of length one or two separates them. *)
  differs "a*" "a+";
  differs "a" "b";
  differs "(ab)*a" "a(ab)*";
  differs "~a" "~b";
  differs "a|b" "a&b";
  (* Emptiness the normal form does not see. *)
  List.iter
    (fun src ->
       check (Printf.sprintf "%S is the empty language" src) (is_empty_language (p src));
       check
         (Printf.sprintf "%S is not the empty term" src)
         (not (Ast.is_empty (to_ast (p src)))))
    [ "a&~a"; "a.*&b.*"; "a*&~(a*)"; "~(.*)"; "ab&ba" ];
  (* Emptiness the normal form does see, through the intersection of
     two disjoint classes. Cheap, and the answer has to be the same. *)
  List.iter
    (fun src ->
       check (Printf.sprintf "%S is the empty language" src) (is_empty_language (p src)))
    [ "a&b"; "(a&b)c" ];
  List.iter
    (fun src ->
       check
         (Printf.sprintf "%S is not the empty language" src)
         (not (is_empty_language (p src))))
    [ "a"; ""; "~a"; "a*"; "a|b"; "~(a&~a)"; ".*" ];
  (* Structurally empty, the one case that derives nothing. *)
  check "the empty term is the empty language" (is_empty_language empty);
  check "eps is not" (not (is_empty_language eps));
  (* Two automata far from minimal, where the product is the size of
     the two multiplied and the traversal is their sum. Both are [a*],
     as a 63-state cycle and a 64-state one: 4958 pairs as a product,
     127 with the union-find. If the union-find ever stops collapsing
     them, this check takes far too long to finish. *)
  let a = singleton_char 'a' in
  let rep n = seqs (List.init n (fun _ -> a)) in
  let cycle n = seq (star (rep n)) (star a) in
  check "(a{64})*a* is a*" (equivalent (cycle 64) (star a));
  check "(a{63})*a* is (a{64})*a*" (equivalent (cycle 63) (cycle 64));
  check "and they are different terms" (not (same_form (cycle 63) (cycle 64)))
;;

(* -- state budgets --------------------------------------------------------- *)

(* The unbounded forms are the bounded ones at [max_int], so agreeing
   with a generous bound is the whole of what makes that safe. The
   tight bounds check the other half, that the budget stops the
   traversal rather than being ignored. *)
let () =
  let a = singleton_char 'a'
  and b = singleton_char 'b' in
  let dots k = List.init k (fun _ -> any) in
  let toks = [ 0, seqs (star any :: a :: dots 6) ] in
  let d = Dfa.of_tokens toks in
  let n = Dfa.num_states d in
  check "the budget test has an automaton to bound" (n = 128);
  check
    "of_tokens_within with room agrees with of_tokens"
    (match Dfa.of_tokens_within ~max_states:(n + 10) toks with
     | Some d' -> same_dfa d d'
     | None -> false);
  check
    "of_tokens_within at exactly the state count succeeds"
    (match Dfa.of_tokens_within ~max_states:n toks with
     | Some d' -> same_dfa d d'
     | None -> false);
  check
    "of_tokens_within one state short gives None"
    (Dfa.of_tokens_within ~max_states:(n - 1) toks = None);
  List.iter
    (fun m ->
       check
         (Printf.sprintf "of_tokens_within at %d gives None" m)
         (Dfa.of_tokens_within ~max_states:m toks = None))
    [ 1; 0; -1 ];
  (* The case the budget exists for. [.*a.{16}] is 131072 states and
     [.*a.{20}] two million, so if the bound were ignored this check
     would take the suite from a second to minutes rather than
     failing. *)
  check
    "a 131072 state automaton is refused at 1000"
    (Dfa.of_tokens_within ~max_states:1000 [ 0, seqs (star any :: a :: dots 16) ] = None);
  check
    "a two million state automaton is refused at 1000"
    (Dfa.of_tokens_within ~max_states:1000 [ 0, seqs (star any :: a :: dots 20) ] = None);
  (* The two decisions, the same way round. [.*a.{6}] is 128 states,
     which both traverse in a millisecond or two; the automata above
     are for the construction bound, where the budget makes the size
     free. *)
  let mid = seqs (star any :: a :: dots 6) in
  check
    "equivalent_within with room agrees with equivalent"
    (equivalent_within ~max_states:100_000 mid (inter mid (star any)) = Some true);
  check
    "equivalent_within under a tight bound gives None"
    (equivalent_within ~max_states:10 mid (inter mid (star any)) = None);
  check
    "is_empty_language_within with room agrees"
    (is_empty_language_within ~max_states:100_000 (inter mid (complement mid)) = Some true);
  check
    "is_empty_language_within under a tight bound gives None"
    (is_empty_language_within ~max_states:10 (inter mid (complement mid)) = None);
  (* A bound of zero still answers where nothing has to be traversed,
     the nullable test coming before the budget. *)
  check
    "a nullable disagreement answers at a bound of zero"
    (equivalent_within ~max_states:0 eps a = Some false);
  check
    "a nullable root answers at a bound of zero"
    (is_empty_language_within ~max_states:0 eps = Some false);
  check
    "an empty term answers at a bound of zero"
    (is_empty_language_within ~max_states:0 empty = Some true);
  check "and a pair needing a step does not" (equivalent_within ~max_states:0 a b = None)
;;

(* Over random terms: a generous bound always reproduces the unbounded
   answer, and some bound in between is [None] on both, so the budget
   is doing something at both ends. *)
let () =
  let st = Random.State.make [| 2718281 |] in
  let bounded_agrees = ref 0
  and tight_refused = ref 0 in
  for _ = 1 to 400 do
    let r, _ = gen ~alphabet st 3 in
    let s, _ = gen ~alphabet st 3 in
    let ar = to_ast r
    and as_ = to_ast s in
    check
      "equivalent_within at max_int is equivalent"
      (Ast.equivalent_within ~max_states:max_int ar as_ = Some (Ast.equivalent ar as_));
    check
      "is_empty_language_within at max_int is is_empty_language"
      (Ast.is_empty_language_within ~max_states:max_int ar
       = Some (Ast.is_empty_language ar));
    incr bounded_agrees;
    (* One state of budget is not enough for most pairs, and never
       gives a wrong answer when it is. *)
    match Ast.equivalent_within ~max_states:1 ar as_ with
    | None -> incr tight_refused
    | Some answer ->
      check "a bounded answer is never wrong" (answer = Ast.equivalent ar as_)
  done;
  check
    (Printf.sprintf
       "the budget refuses some pairs at one state (%d of %d)"
       !tight_refused
       !bounded_agrees)
    (!tight_refused > 0 && !tight_refused < !bounded_agrees)
;;

(* -- the intern table gives memory back ------------------------------------ *)

(* Entries are weak, so transient terms are collected on their own. What
   used to be retained was the bucket array around them: it was sized
   from the number of terms interned since the last resize, and only
   ever doubled, so its footprint tracked cumulative interning rather
   than live entries. Interning half a million distinct terms and
   dropping them all should leave the heap roughly where it started.

   Compacting more than once is deliberate. The shrink runs at the end
   of a major cycle, so the array it releases is still garbage at that
   point and is not reclaimed until the following one. *)
let () =
  let compact () =
    for _ = 1 to 3 do
      Gc.compact ()
    done
  in
  let live_mb () =
    float_of_int (Gc.quick_stat ()).Gc.heap_words
    *. float_of_int (Sys.word_size / 8)
    /. 1048576.
  in
  compact ();
  let before = live_mb () in
  let last = ref Ast.eps in
  for i = 0 to 499_999 do
    (* Plane 2 either side, so no surrogate is ever asked for. *)
    last
    := Ast.star
         (Ast.seq
            (Ast.singleton (0x20000 + (i land 0xFFF)))
            (Ast.singleton (0x30000 + (i lsr 12))))
  done;
  ignore (Ast.tag !last);
  compact ();
  let after = live_mb () in
  (* Before the fix this grew by about 12.5 MB at this size; after it,
     by nothing measurable. The threshold sits an order of magnitude
     clear of both. *)
  check
    (Printf.sprintf "intern table gives memory back (%.2f -> %.2f MB)" before after)
    (after -. before < 4.0)
;;

(* [rehash] re-measures the table only when something is interned, so a
   program that builds a large automaton, drops it and then stops
   interning holds the bucket array at its peak. [clear_cache] is the
   release. Last in the file: it orphans every node interned before it,
   so nothing built earlier may be used after. *)
let () =
  let live_mb () =
    float_of_int (Gc.quick_stat ()).Gc.heap_words
    *. float_of_int (Sys.word_size / 8)
    /. 1048576.
  in
  (* [.*a.{16}] — 2^17 states, enough that the table is far past the
     size it starts at. Built and dropped in one expression. *)
  let states =
    Dfa.num_states
      (Dfa.of_tokens
         [ 0, seqs (star any :: singleton (Char.code 'a') :: List.init 16 (fun _ -> any))
         ])
  in
  check "the automaton is the size the measurement assumes" (states = 131072);
  Gc.compact ();
  let held = live_mb () in
  Ast.clear_cache ();
  Gc.compact ();
  let cleared = live_mb () in
  (* Measured: 4.88 MB held, 0.57 MB after, against a 0.57 MB baseline. *)
  check
    (Printf.sprintf "clear_cache releases the table (%.2f -> %.2f MB)" held cleared)
    (cleared < held -. 2.0);
  (* The constants are put back, so interning still finds them. *)
  check "eps survives a clear" (Ast.is_eps (to_ast eps));
  check "empty survives a clear" (Ast.is_empty (to_ast empty));
  check "any survives a clear" (Ast.equal (to_ast any) Ast.any)
;;

let () =
  if !failures = 0
  then print_endline "all property checks passed"
  else (
    Printf.printf "%d failures\n" !failures;
    exit 1)
;;
