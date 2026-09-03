(* -- surface syntax -----------------------------------------------------------

   The regex users write, keeping the shape they wrote it in. Two ways
   out:

   1. [to_ast] expands into the normal form the engine derives over.
      The expansion is structural, a constructor at a time.

   2. [to_oniguruma] emits regex source, traversing the tree directly.
      [Plus], [Opt] and [Neg_chars] are separate constructors here, so
      they map straight to [+], [?] and [\[^...\]].

   Smart constructors take the local algebraic simplifications
   ([seq empty x = empty], [alt empty x = x] and so on) and leave the
   rest as written.
   -------------------------------------------------------------------------- *)

type t =
  | Chars of Ucharset.t
  | Neg_chars of Ucharset.t
  | Eps
  | Seq of t list
  | Alt of t list
  | Star of t
  | Plus of t
  | Opt of t
  | Complement of t
  | Inter of t list

(* -- predicates ------------------------------------------------------------ *)

let is_empty = function
  | Chars chars -> Ucharset.is_empty chars
  | Neg_chars chars -> Ucharset.is_empty (Ucharset.comp chars)
  | _ -> false
;;

let is_eps = function
  | Eps -> true
  | _ -> false
;;

let charset_of = function
  | Chars chars -> Some chars
  | Neg_chars chars -> Some (Ucharset.comp chars)
  | _ -> None
;;

(* -- constants and constructors -------------------------------------------- *)

let empty = Chars Ucharset.empty
let eps = Eps
let any = Chars Ucharset.all
let chars chars = if Ucharset.is_empty chars then empty else Chars chars
let singleton cp = chars (Ucharset.singleton cp)
let singleton_char ch = chars (Ucharset.singleton_char ch)
let singleton_uchar uch = chars (Ucharset.singleton_uchar uch)
let range ~lo ~hi = chars (Ucharset.range ~lo ~hi)
let range_char ~lo ~hi = chars (Ucharset.range_char ~lo ~hi)
let range_uchar ~lo ~hi = chars (Ucharset.range_uchar ~lo ~hi)
let not_chars chars = Neg_chars chars
let not_singleton cp = not_chars (Ucharset.singleton cp)
let not_singleton_char ch = not_chars (Ucharset.singleton_char ch)
let not_singleton_uchar uch = not_chars (Ucharset.singleton_uchar uch)
let not_range ~lo ~hi = not_chars (Ucharset.range ~lo ~hi)
let not_range_char ~lo ~hi = not_chars (Ucharset.range_char ~lo ~hi)
let not_range_uchar ~lo ~hi = not_chars (Ucharset.range_uchar ~lo ~hi)

(* -- character class helpers -------------------------------------------------

   The common shapes of a single codepoint class, so building one does
   not mean reaching for [Ucharset]. Unsuffixed takes codepoints, as
   [singleton] and [range] do.
   -------------------------------------------------------------------------- *)

let chars_of_list cps = chars (Ucharset.of_list cps)
let chars_of_char_list cs = chars (Ucharset.of_char_list cs)
let chars_of_uchar_list us = chars (Ucharset.of_uchar_list us)

let ranges_set to_set rs =
  Ucharset.union_list (List.map (fun (lo, hi) -> to_set lo hi) rs)
;;

let chars_in_ranges rs = chars (ranges_set (fun lo hi -> Ucharset.range ~lo ~hi) rs)

let chars_in_char_ranges rs =
  chars (ranges_set (fun lo hi -> Ucharset.range_char ~lo ~hi) rs)
;;

let chars_in_uchar_ranges rs =
  chars (ranges_set (fun lo hi -> Ucharset.range_uchar ~lo ~hi) rs)
;;

(* The trailing [unit] guards against a partial application when both
   labels are left off. *)
let one_of ?(singles = []) ?(ranges = []) () =
  chars
    (Ucharset.union
       (Ucharset.of_list singles)
       (ranges_set (fun lo hi -> Ucharset.range ~lo ~hi) ranges))
;;

let one_of_char ?(singles = []) ?(ranges = []) () =
  chars
    (Ucharset.union
       (Ucharset.of_char_list singles)
       (ranges_set (fun lo hi -> Ucharset.range_char ~lo ~hi) ranges))
;;

let one_of_uchar ?(singles = []) ?(ranges = []) () =
  chars
    (Ucharset.union
       (Ucharset.of_uchar_list singles)
       (ranges_set (fun lo hi -> Ucharset.range_uchar ~lo ~hi) ranges))
;;

(* [String.get_utf_8_uchar] answers U+FFFD on a bad byte rather than
   failing, which would make [str] silently denote a regex the caller
   did not write. [of_string] rejects the same bytes, so this does
   too. *)
let str s =
  let cs = ref []
  and i = ref 0
  and n = String.length s in
  while !i < n do
    let d = String.get_utf_8_uchar s !i in
    if not (Uchar.utf_decode_is_valid d)
    then invalid_arg "Redfa.Regex.str: malformed UTF-8";
    cs := singleton (Uchar.to_int (Uchar.utf_decode_uchar d)) :: !cs;
    i := !i + Uchar.utf_decode_length d
  done;
  match !cs with
  | [] -> eps
  | [ x ] -> x
  | _ -> Seq (List.rev !cs)
;;

let seq_children = function
  | Seq xs -> xs
  | t -> [ t ]
;;

let seq a b =
  if is_empty a || is_empty b
  then empty
  else if is_eps a
  then b
  else if is_eps b
  then a
  else Seq (seq_children a @ seq_children b)
;;

let seqs ts =
  if List.exists is_empty ts
  then empty
  else (
    let flat = List.concat_map seq_children ts in
    let nonemp = List.filter (fun t -> not (is_eps t)) flat in
    match nonemp with
    | [] -> eps
    | [ x ] -> x
    | xs -> Seq xs)
;;

let alt_children = function
  | Alt xs -> xs
  | t -> [ t ]
;;

let alts ts =
  let flat = List.concat_map alt_children ts in
  let nonemp = List.filter (fun t -> not (is_empty t)) flat in
  (* Merge the positive [Chars] children into one and leave it where
     the first of them sat, so [alt (singleton_char 'a')
     (singleton_char 'b')] emits [\[ab\]]. [Neg_chars] children stay as
     they are: folding one through its complement turns [\[^b\]] into a
     class spanning the codespace. *)
  let positives =
    List.filter_map
      (function
        | Chars c -> Some c
        | _ -> None)
      nonemp
  in
  let nonemp =
    match positives with
    | [] | [ _ ] -> nonemp
    | cs ->
      let merged = Ucharset.union_list cs in
      let placed = ref false in
      List.filter_map
        (function
          | Chars _ when !placed -> None
          | Chars _ ->
            placed := true;
            Some (Chars merged)
          | t -> Some t)
        nonemp
  in
  match nonemp with
  | [] -> empty
  | [ x ] -> x
  | xs -> Alt xs
;;

let alt a b = alts [ a; b ]

(* A star absorbs an inner star, plus or opt. *)
let star t =
  if is_empty t || is_eps t
  then eps
  else (
    match t with
    | Star _ -> t
    | Plus x | Opt x -> Star x
    | _ -> Star t)
;;

(* A plus over a star or a plus is that same term; over an opt it
   widens to a star. *)
let plus t =
  if is_empty t
  then empty
  else if is_eps t
  then eps
  else (
    match t with
    | Star _ | Plus _ -> t
    | Opt x -> Star x
    | _ -> Plus t)
;;

(* An opt over a star or an opt is that same term; over a plus it
   widens to a star. *)
let opt t =
  if is_empty t || is_eps t
  then eps
  else (
    match t with
    | Star _ | Opt _ -> t
    | Plus x -> Star x
    | _ -> Opt t)
;;

let complement = function
  | Complement x -> x
  | t -> Complement t
;;

(* Whichever spelling of [c] has fewer intervals, so an intersection of
   negated classes stays negated. *)
let chars_or_neg c =
  let d = Ucharset.comp c in
  if Ucharset.num_intervals d < Ucharset.num_intervals c then Neg_chars d else chars c
;;

let inters ts =
  match ts with
  | [] -> star any (* identity for intersection: the universal language *)
  | _ ->
    let flat =
      List.concat_map
        (function
          | Inter xs -> xs
          | t -> [ t ])
        ts
    in
    if List.exists is_empty flat
    then empty
    else (
      let cs_opts = List.map charset_of flat in
      if List.for_all Option.is_some cs_opts
      then (
        match List.map Option.get cs_opts with
        | [] -> star any
        | first :: rest -> chars_or_neg (List.fold_left Ucharset.inter first rest))
      else (
        match flat with
        | [] -> empty
        | [ x ] -> x
        | xs -> Inter xs))
;;

let inter a b = inters [ a; b ]

(* -- lowering to the normal form ------------------------------------------- *)

let rec to_ast = function
  | Chars chars -> Ast.chars chars
  | Neg_chars chars ->
    (* Any single codepoint outside [c], so the set's complement. *)
    Ast.chars (Ucharset.comp chars)
  | Eps -> Ast.eps
  | Seq xs -> Ast.seqs (List.map to_ast xs)
  | Alt xs -> Ast.alts (List.map to_ast xs)
  | Star x -> Ast.star (to_ast x)
  | Plus x ->
    let r = to_ast x in
    Ast.seq r (Ast.star r)
  | Opt x -> Ast.alt (to_ast x) Ast.eps
  | Complement x -> Ast.complement (to_ast x)
  | Inter xs -> Ast.inters (List.map to_ast xs)
;;

let rec is_nullable = function
  | Chars _ | Neg_chars _ -> false
  | Eps | Star _ | Opt _ -> true
  | Seq xs | Inter xs -> List.for_all is_nullable xs
  | Alt xs -> List.exists is_nullable xs
  | Plus x -> is_nullable x
  | Complement x -> not (is_nullable x)
;;

(* -- parsing ------------------------------------------------------------------

   Recursive descent over

     alt    := inter ('|' inter)*
     inter  := concat ('&' concat)*
     concat := prefix*
     prefix := '~' prefix | repeat
     repeat := atom ('*' | '+' | '?')*
     atom   := '(' alt ')' | '[' class ']' | '.' | escape | literal

   loosest first, matching [prec_of]. An empty concatenation is [eps],
   so [()] and the branches of [a||b] parse.

   Escapes are [\t], [\n], [\r], [\u{HHHH}], the shorthand classes
   [\d], [\w], [\s] and their negations, and a backslash before any
   ASCII punctuation for that character literally. Inside a class,
   [\]] and [\-] carry their literal.
   -------------------------------------------------------------------------- *)

type error =
  { pos : int (* byte offset into the source *)
  ; msg : string
  }

exception Fail of error

type cursor =
  { src : string
  ; mutable at : int
  }

let fail_at pos msg = raise (Fail { pos; msg })
let fail c msg = fail_at c.at msg
let eof c = c.at >= String.length c.src
let peek c = if eof c then None else Some (String.unsafe_get c.src c.at)

let bump c =
  let ch = c.src.[c.at] in
  c.at <- c.at + 1;
  ch
;;

let accept c ch =
  match peek c with
  | Some x when x = ch ->
    c.at <- c.at + 1;
    true
  | _ -> false
;;

let describe c =
  match peek c with
  | None -> "end of input"
  | Some ch -> Printf.sprintf "%C" ch
;;

let expect c ch what =
  if not (accept c ch)
  then fail c (Printf.sprintf "expected %C %s, found %s" ch what (describe c))
;;

(* Shorthand classes, over the ASCII ranges they conventionally name. *)
let digit_set = Ucharset.range ~lo:(Char.code '0') ~hi:(Char.code '9')

let word_set =
  Ucharset.union_list
    [ Ucharset.range ~lo:(Char.code 'a') ~hi:(Char.code 'z')
    ; Ucharset.range ~lo:(Char.code 'A') ~hi:(Char.code 'Z')
    ; digit_set
    ; Ucharset.singleton_char '_'
    ]
;;

let space_set = Ucharset.of_utf_8_string " \t\n\r\012"

let hex_value = function
  | '0' .. '9' as ch -> Some (Char.code ch - Char.code '0')
  | 'a' .. 'f' as ch -> Some (Char.code ch - Char.code 'a' + 10)
  | 'A' .. 'F' as ch -> Some (Char.code ch - Char.code 'A' + 10)
  | _ -> None
;;

(* [\u{HHHH}], the braces required so the digit run has an end.

   The digits are folded here rather than passed to [int_of_string],
   which takes [_] as a separator, so [\u{6_1}] parsed as [a]. The
   fold stops climbing once past [max_codepoint], so a long run stays
   in range of an [int] and still fails the test below. *)
let parse_unicode_escape c =
  let start = c.at - 2 in
  expect c '{' "after \\u";
  let from = c.at in
  while (not (eof c)) && peek c <> Some '}' do
    ignore (bump c)
  done;
  if eof c then fail_at start "unterminated \\u escape, expected '}'";
  let digits = String.sub c.src from (c.at - from) in
  ignore (bump c);
  if digits = "" then fail_at start "empty \\u escape";
  let cp = ref 0
  and hex = ref true in
  String.iter
    (fun ch ->
       match hex_value ch with
       | None -> hex := false
       | Some v -> if !cp <= 0x10FFFF then cp := (!cp * 16) + v)
    digits;
  if not !hex then fail_at start (Printf.sprintf "invalid hex in \\u escape: %S" digits);
  if !cp > 0x10FFFF || (!cp >= 0xD800 && !cp <= 0xDFFF)
  then fail_at start (Printf.sprintf "\\u{%s} is not a Unicode scalar value" digits);
  !cp
;;

(* Printable ASCII outside the letters and digits; the characters the
   grammar reserves, and the rest of the ASCII punctuation with them.
   [src] writes space as itself and a control as [\t] or [\u{HH}], so
   a backslash before one comes from a caller writing it. *)
let is_ascii_punct ch =
  let c = Char.code ch in
  c > 0x20
  && c < 0x7F
  && (not (c >= Char.code '0' && c <= Char.code '9'))
  && not ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
;;

(* Escapes yielding a single codepoint. A shorthand class returns
   [None] here and is handled by the caller, which knows whether a set
   is admissible at that position. *)
let parse_escape_cp c =
  let start = c.at - 1 in
  if eof c then fail_at start "trailing backslash";
  match bump c with
  | 't' -> Some 0x09
  | 'n' -> Some 0x0A
  | 'r' -> Some 0x0D
  | 'f' -> Some 0x0C
  | '0' -> Some 0x00
  | 'u' -> Some (parse_unicode_escape c)
  | 'd' | 'w' | 's' | 'D' | 'W' | 'S' -> None
  | ch when is_ascii_punct ch -> Some (Char.code ch)
  (* Space, the C0 controls and DEL stand for themselves, so a
     backslash before one is a mistake. Named by codepoint, which
     keeps a control character out of the message. *)
  | ch when Char.code ch < 0x21 || Char.code ch = 0x7F ->
    fail_at start (Printf.sprintf "unknown escape: U+%04X" (Char.code ch))
  (* The byte after the backslash can be the lead of a multi-byte
     character, so decode it; [%c] on the raw byte made an error
     message that was itself malformed UTF-8. *)
  | _ ->
    let d = String.get_utf_8_uchar c.src (start + 1) in
    if not (Uchar.utf_decode_is_valid d) then fail_at (start + 1) "malformed UTF-8";
    let b = Buffer.create 16 in
    Buffer.add_string b "unknown escape \\";
    Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar d);
    fail_at start (Buffer.contents b)
;;

let shorthand_set = function
  | 'd' -> digit_set
  | 'w' -> word_set
  | 's' -> space_set
  | 'D' -> Ucharset.comp digit_set
  | 'W' -> Ucharset.comp word_set
  | 'S' -> Ucharset.comp space_set
  | _ -> assert false
;;

(* One codepoint at the cursor, decoding UTF-8. *)
let parse_literal_cp c =
  let d = String.get_utf_8_uchar c.src c.at in
  if not (Uchar.utf_decode_is_valid d) then fail c "malformed UTF-8";
  c.at <- c.at + Uchar.utf_decode_length d;
  Uchar.to_int (Uchar.utf_decode_uchar d)
;;

(* A class member is a codepoint, a shorthand set, or a range between
   two codepoints. *)
let parse_class_item c =
  match peek c with
  | Some '\\' ->
    ignore (bump c);
    let here = c.at - 1 in
    (match parse_escape_cp c with
     | Some cp -> `Cp cp
     | None -> `Set (shorthand_set c.src.[here + 1]))
  | _ -> `Cp (parse_literal_cp c)
;;

let parse_class c =
  let open_at = c.at - 1 in
  let negated = accept c '^' in
  let acc = ref [] in
  let saw = ref false in
  let rec loop () =
    if eof c then fail_at open_at "unterminated character class, expected ']'";
    if accept c ']'
    then ()
    else (
      saw := true;
      let lo_at = c.at in
      (match parse_class_item c with
       | `Set s -> acc := s :: !acc
       | `Cp lo ->
         if peek c = Some '-' && c.at + 1 < String.length c.src && c.src.[c.at + 1] <> ']'
         then (
           ignore (bump c);
           match parse_class_item c with
           | `Set _ -> fail c "a shorthand class cannot be the end of a range"
           | `Cp hi ->
             if hi < lo
             then
               fail_at
                 lo_at
                 (Printf.sprintf "range runs backwards, U+%04X to U+%04X" lo hi);
             acc := Ucharset.range ~lo ~hi :: !acc)
         else acc := Ucharset.singleton lo :: !acc);
      loop ())
  in
  loop ();
  if not !saw then fail_at open_at "empty character class";
  let set = Ucharset.union_list !acc in
  if negated then not_chars set else chars set
;;

let rec parse_alt c =
  let first = parse_inter c in
  let acc = ref [ first ] in
  while accept c '|' do
    acc := parse_inter c :: !acc
  done;
  match !acc with
  | [ x ] -> x
  | xs -> alts (List.rev xs)

and parse_inter c =
  let first = parse_concat c in
  let acc = ref [ first ] in
  while accept c '&' do
    acc := parse_concat c :: !acc
  done;
  match !acc with
  | [ x ] -> x
  | xs -> inters (List.rev xs)

and parse_concat c =
  let acc = ref [] in
  let rec loop () =
    match peek c with
    | None | Some '|' | Some '&' | Some ')' -> ()
    | Some _ ->
      acc := parse_prefix c :: !acc;
      loop ()
  in
  loop ();
  match !acc with
  | [] -> eps
  | [ x ] -> x
  | xs -> seqs (List.rev xs)

and parse_prefix c = if accept c '~' then complement (parse_prefix c) else parse_repeat c

and parse_repeat c =
  let t = ref (parse_atom c) in
  let rec loop () =
    match peek c with
    | Some '*' ->
      ignore (bump c);
      t := star !t;
      loop ()
    | Some '+' ->
      ignore (bump c);
      t := plus !t;
      loop ()
    | Some '?' ->
      ignore (bump c);
      t := opt !t;
      loop ()
    | _ -> ()
  in
  loop ();
  !t

and parse_atom c =
  match peek c with
  | None -> fail c "expected an expression, found end of input"
  | Some '(' ->
    let open_at = c.at in
    ignore (bump c);
    (* [(?:] as a synonym for [(], so Oniguruma output parses back. *)
    if peek c = Some '?' && c.at + 1 < String.length c.src && c.src.[c.at + 1] = ':'
    then c.at <- c.at + 2;
    let inner = parse_alt c in
    if not (accept c ')')
    then fail_at open_at (Printf.sprintf "unclosed '(', found %s" (describe c));
    inner
  | Some ')' -> fail c "unbalanced ')'"
  | Some '[' ->
    ignore (bump c);
    parse_class c
  | Some '.' ->
    ignore (bump c);
    any
  | Some (('*' | '+' | '?') as ch) ->
    fail c (Printf.sprintf "nothing for %C to repeat" ch)
  | Some '\\' ->
    let here = c.at in
    ignore (bump c);
    (match parse_escape_cp c with
     | Some cp -> singleton cp
     | None -> chars (shorthand_set c.src.[here + 1]))
  | Some _ -> singleton (parse_literal_cp c)
;;

let of_string s =
  let c = { src = s; at = 0 } in
  match parse_alt c with
  | t -> if eof c then Ok t else Error { pos = c.at; msg = "unbalanced ')'" }
  | exception Fail e -> Error e
;;

(* The source with a caret under the offending byte. *)
let error_to_string src { pos; msg } =
  let pos = if pos > String.length src then String.length src else pos in
  Printf.sprintf "%s\n%s^ %s" src (String.make pos ' ') msg
;;

(* Loosest to tightest: alternation, intersection, concatenation,
   complement, the postfix operators, then atoms. *)
let prec_of = function
  | Star _ | Plus _ | Opt _ -> 5
  | Complement _ -> 4
  | Seq _ -> 3
  | Inter _ -> 2
  | Alt _ -> 1
  | Chars _ | Neg_chars _ | Eps -> 6
;;

(* -- rendering ----------------------------------------------------------------

   The source form {!of_string} reads back. Escapes the characters the
   grammar gives meaning to, and parenthesises by the same precedence
   {!pp} uses.
   -------------------------------------------------------------------------- *)

let src_cp ~in_class buf cp =
  match cp with
  | 0x09 -> Buffer.add_string buf "\\t"
  | 0x0A -> Buffer.add_string buf "\\n"
  | 0x0D -> Buffer.add_string buf "\\r"
  | _ when cp >= 0x20 && cp <= 0x7E ->
    let ch = Char.chr cp in
    let special =
      if in_class
      then (
        match ch with
        | ']' | '-' | '^' | '\\' -> true
        | _ -> false)
      else (
        match ch with
        | '|' | '&' | '(' | ')' | '[' | ']' | '*' | '+' | '?' | '.' | '~' | '\\' -> true
        | _ -> false)
    in
    if special then Buffer.add_char buf '\\';
    Buffer.add_char buf ch
  | _ -> Buffer.add_string buf (Printf.sprintf "\\u{%X}" cp)
;;

let src_class ~negated buf cs =
  Buffer.add_char buf '[';
  if negated then Buffer.add_char buf '^';
  Ucharset.iter_intervals
    (fun lo hi ->
       src_cp ~in_class:true buf lo;
       if hi > lo
       then (
         if hi - lo > 1 then Buffer.add_char buf '-';
         src_cp ~in_class:true buf hi))
    cs;
  Buffer.add_char buf ']'
;;

(* The empty language has no literal of its own, so it goes out as a
   class negating the whole codespace. *)
let src_charset buf cs =
  if Ucharset.is_empty cs
  then src_class ~negated:true buf Ucharset.all
  else if Ucharset.is_all cs
  then Buffer.add_char buf '.'
  else if Ucharset.is_singleton cs
  then (
    match Ucharset.choose_opt cs with
    | Some cp -> src_cp ~in_class:false buf cp
    | None -> assert false)
  else src_class ~negated:false buf cs
;;

let rec src prec buf t =
  if prec_of t < prec
  then (
    Buffer.add_char buf '(';
    src 0 buf t;
    Buffer.add_char buf ')')
  else (
    let sep ch p xs =
      List.iteri
        (fun i x ->
           (match ch with
            | Some c when i > 0 -> Buffer.add_char buf c
            | _ -> ());
           src p buf x)
        xs
    in
    match t with
    | Chars cs -> src_charset buf cs
    | Neg_chars cs ->
      if Ucharset.is_empty cs
      then Buffer.add_char buf '.'
      else src_class ~negated:true buf cs
    | Eps -> Buffer.add_string buf "()"
    (* An empty alternation is the empty language and an empty
       intersection is every string, which is what [to_ast] gives
       them. Neither is reachable through the smart constructors, but
       both are constructible, and [sep] over no children would render
       each as the empty source, which reads back as [eps]. *)
    | Alt [] -> src_charset buf Ucharset.empty
    | Inter [] -> Buffer.add_string buf ".*"
    | Seq xs -> sep None 3 xs
    | Alt xs -> sep (Some '|') 1 xs
    | Inter xs -> sep (Some '&') 2 xs
    | Complement x ->
      Buffer.add_char buf '~';
      src 4 buf x
    | Star x ->
      src 5 buf x;
      Buffer.add_char buf '*'
    | Plus x ->
      src 5 buf x;
      Buffer.add_char buf '+'
    | Opt x ->
      src 5 buf x;
      Buffer.add_char buf '?')
;;

let to_string t =
  let buf = Buffer.create 64 in
  src 0 buf t;
  Buffer.contents buf
;;

(* -- Oniguruma emission ---------------------------------------------------- *)

exception Emission_failed of string

(* Tab, newline and carriage return go out as [\t], [\n] and [\r].
   Other codepoints outside printable ASCII take the [\u\{HHHH\}]
   form. *)
let cp_in_charset buf cp =
  match cp with
  | 0x09 -> Buffer.add_string buf "\\t"
  | 0x0A -> Buffer.add_string buf "\\n"
  | 0x0D -> Buffer.add_string buf "\\r"
  | _ when cp >= 0x20 && cp <= 0x7E ->
    let c = Char.chr cp in
    (* [ and : are escaped too: an unescaped "[:" inside a class
       opens a POSIX bracket expression ([:alpha:]) in Oniguruma. *)
    (match c with
     | ']' | '\\' | '^' | '-' | '[' | ':' ->
       Buffer.add_char buf '\\';
       Buffer.add_char buf c
     | _ -> Buffer.add_char buf c)
  | _ -> Buffer.add_string buf (Printf.sprintf "\\u{%X}" cp)
;;

let cp_outside_charset buf cp =
  match cp with
  | 0x09 -> Buffer.add_string buf "\\t"
  | 0x0A -> Buffer.add_string buf "\\n"
  | 0x0D -> Buffer.add_string buf "\\r"
  | _ when cp >= 0x20 && cp <= 0x7E ->
    let c = Char.chr cp in
    (match c with
     (* [&] and [~] are redfa's intersection and complement operators, so
        an unescaped one reparses as a different language, not a literal.
        Oniguruma takes a backslash before either as the character. *)
     | '\\'
     | '.'
     | '['
     | ']'
     | '('
     | ')'
     | '{'
     | '}'
     | '*'
     | '+'
     | '?'
     | '|'
     | '^'
     | '$'
     | '&'
     | '~' ->
       Buffer.add_char buf '\\';
       Buffer.add_char buf c
     | _ -> Buffer.add_char buf c)
  | _ -> Buffer.add_string buf (Printf.sprintf "\\u{%X}" cp)
;;

let emit_interval buf lo hi =
  if lo = hi
  then cp_in_charset buf lo
  else if hi - lo = 1
  then (
    cp_in_charset buf lo;
    cp_in_charset buf hi)
  else (
    cp_in_charset buf lo;
    Buffer.add_char buf '-';
    cp_in_charset buf hi)
;;

let emit_charset_brackets ~negated buf cs =
  Buffer.add_char buf '[';
  if negated then Buffer.add_char buf '^';
  Ucharset.iter_intervals (fun lo hi -> emit_interval buf lo hi) cs;
  Buffer.add_char buf ']'
;;

let emit_charset buf cs =
  if Ucharset.is_empty cs
  then raise (Emission_failed "empty language has no Oniguruma representation")
  else if Ucharset.is_singleton cs
  then (
    match Ucharset.choose_opt cs with
    | Some cp -> cp_outside_charset buf cp
    | None -> assert false)
  else emit_charset_brackets ~negated:false buf cs
;;

(* [Neg_chars cs] denotes [comp cs]. Negating the empty set goes out
   as a positive full range class, and negating the whole codespace
   raises. *)
let emit_charset_negated buf cs =
  if Ucharset.is_empty cs
  then emit_charset_brackets ~negated:false buf Ucharset.all
  else if Ucharset.is_empty (Ucharset.comp cs)
  then raise (Emission_failed "empty language has no Oniguruma representation")
  else emit_charset_brackets ~negated:true buf cs
;;

let rec emit_top buf c =
  match c with
  | Chars chars -> emit_charset buf chars
  | Neg_chars chars -> emit_charset_negated buf chars
  | Eps -> ()
  | Seq [] -> ()
  | Seq xs -> List.iter (emit_factor buf) xs
  (* [Alt []] is the empty language. *)
  | Alt [] ->
    raise (Emission_failed "empty-language alternation has no Oniguruma representation")
  | Alt [ x ] -> emit_top buf x
  | Alt (x :: rest) ->
    emit_top buf x;
    List.iter
      (fun y ->
         Buffer.add_char buf '|';
         emit_top buf y)
      rest
  | Star x ->
    emit_atom buf x;
    Buffer.add_char buf '*'
  | Plus x ->
    emit_atom buf x;
    Buffer.add_char buf '+'
  | Opt x ->
    emit_atom buf x;
    Buffer.add_char buf '?'
  | Complement _ ->
    (* A complement takes in the empty string and strings of any
       length, which Oniguruma has no form for. *)
    raise
      (Emission_failed
         "complement is over the language, which Oniguruma has no form for; for a \
          negated character class use not_chars")
  | Inter xs ->
    let cs_opts = List.map charset_of xs in
    if List.for_all Option.is_some cs_opts
    then (
      match List.map Option.get cs_opts with
      | [] -> raise (Emission_failed "empty intersection is unreachable here")
      | first :: rest ->
        let combined = List.fold_left Ucharset.inter first rest in
        emit_charset buf combined)
    else
      raise
        (Emission_failed
           "an intersection over anything but character classes has no Oniguruma form")

and emit_factor buf c =
  match c with
  | Alt _ ->
    Buffer.add_string buf "(?:";
    emit_top buf c;
    Buffer.add_string buf ")"
  | _ -> emit_top buf c

and emit_atom buf c =
  match c with
  | Chars _ | Neg_chars _ -> emit_top buf c
  | Inter xs when List.for_all (fun x -> Option.is_some (charset_of x)) xs ->
    (* Inter that lowers to a charset is also atomic. *)
    emit_top buf c
  | _ ->
    Buffer.add_string buf "(?:";
    emit_top buf c;
    Buffer.add_string buf ")"
;;

let to_oniguruma c =
  let buf = Buffer.create 64 in
  try
    emit_top buf c;
    Ok (Buffer.contents buf)
  with
  | Emission_failed msg -> Error msg
;;

(* -- deciding a language --------------------------------------------------- *)

(* Both lower and hand the question to {!Ast}, which carries the
   algorithm and the cost. The structural test [equivalent] used to be
   is still [Ast.equal (to_ast a) (to_ast b)]. *)
let is_empty_language t = Ast.is_empty_language (to_ast t)
let equivalent a b = Ast.equivalent (to_ast a) (to_ast b)

let is_empty_language_within ~max_states t =
  Ast.is_empty_language_within ~max_states (to_ast t)
;;

let equivalent_within ~max_states a b =
  Ast.equivalent_within ~max_states (to_ast a) (to_ast b)
;;

(* -- pretty-printing ------------------------------------------------------- *)

(* [src] writes [Neg_chars] as the atomic class source [[^...]], so
   {!prec_of} ranks it with the atoms. [pp] writes it as a prefix [^]
   instead, which binds and parenthesises like a [Complement]. The two
   paths differ only here. *)
let pp_prec_of = function
  | Neg_chars _ -> 4
  | t -> prec_of t
;;

let rec pp_prec prec ppf t =
  let p = pp_prec_of t in
  if p < prec
  then Format.fprintf ppf "(%a)" (pp_prec 0) t
  else (
    match t with
    | Chars c -> Ucharset.pp ppf c
    (* [^] for the set complement, [~] for the language complement:
       [Neg_chars c] and [Complement (Chars c)] denote different
       things and used to print alike. *)
    | Neg_chars c -> Format.fprintf ppf "^%a" Ucharset.pp c
    | Eps -> Format.fprintf ppf "ε"
    (* Both would otherwise print as nothing, which is what [Seq []]
       prints and is the language [eps], not these. *)
    | Alt [] -> Format.fprintf ppf "∅"
    | Inter [] -> Format.fprintf ppf "Σ*"
    | Seq xs ->
      Format.fprintf
        ppf
        "@[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,") (pp_prec 3))
        xs
    | Alt xs ->
      Format.fprintf
        ppf
        "@[<hov>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ |@ ")
           (pp_prec 1))
        xs
    | Inter xs ->
      Format.fprintf
        ppf
        "@[<hov>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ &@ ")
           (pp_prec 2))
        xs
    | Complement x -> Format.fprintf ppf "~%a" (pp_prec 4) x
    | Star x -> Format.fprintf ppf "%a*" (pp_prec 5) x
    | Plus x -> Format.fprintf ppf "%a+" (pp_prec 5) x
    | Opt x -> Format.fprintf ppf "%a?" (pp_prec 5) x)
;;

let pp ppf t = pp_prec 0 ppf t
