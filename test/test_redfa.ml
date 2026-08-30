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
   terms exercise the dedup and merge paths. *)
let alphabet = [| 'a'; 'b'; 'c'; 'd' |]

let rec gen st depth =
  let leaf () =
    match Random.State.int st 5 with
    | 0 -> empty, fun _ -> false
    | 1 -> eps, fun s -> String.length s = 0
    | 2 -> any, fun s -> String.length s = 1
    | 3 ->
      let c = alphabet.(Random.State.int st (Array.length alphabet)) in
      not_singleton_char c, fun s -> String.length s = 1 && s.[0] <> c
    | _ ->
      let c = alphabet.(Random.State.int st (Array.length alphabet)) in
      singleton (Char.code c), fun s -> String.length s = 1 && s.[0] = c
  in
  if depth <= 0
  then leaf ()
  else (
    let sub () = gen st (depth - 1) in
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
          (fun s -> Array.to_list (Array.map (fun c -> s ^ String.make 1 c) alphabet))
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

(* -- the language the engine computes -------------------------------------- *)

let () =
  let st = Random.State.make [| 20260829 |] in
  for _ = 1 to 3000 do
    let r, oracle = gen st 3 in
    let a = to_ast r in
    List.iter (fun s -> check "eval agrees with oracle" (Ast.eval a s = oracle s)) corpus
  done
;;

(* -- canonical form the smart constructors promise ------------------------- *)

let () =
  let st = Random.State.make [| 4242 |] in
  for _ = 1 to 20_000 do
    let r, _ = gen st 4 in
    let r = to_ast r in
    if Ast.is_alt r
    then (
      let xs = Ast.alt_children r in
      check "Alt sorted distinct" (List.map Ast.tag xs = List.map Ast.tag (Ast.sort_distinct xs));
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
  let a = singleton_char 'a' and b = singleton_char 'b' and c = singleton_char 'c' in
  same "abc" (seqs [ a; b; c ]);
  same "a|bc" (alts [ a; seq b c ]);
  same "(a|b)c" (seq (alts [ a; b ]) c);
  same "a*" (star a);
  same "~a" (complement a);
  same "a&b" (inters [ a; b ]);
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

(* -- source round trips ---------------------------------------------------- *)

let () =
  let st = Random.State.make [| 7 |] in
  for _ = 1 to 4000 do
    let r, _ = gen st 3 in
    (* Rendering and parsing check each other, so a precedence slip on
       either side shows up here. *)
    let src = to_string r in
    (match of_string src with
     | Error e -> check ("to_string produced unparseable source: " ^ src) false
     | Ok back ->
       check
         (Printf.sprintf "round trip %s" src)
         (equivalent r back));
    (* Oniguruma output is a subset of the same syntax, once [(?:] is
       read as a group. *)
    match to_oniguruma r with
    | Error _ -> ()
    | Ok oni ->
      (match of_string oni with
       | Error e ->
         check (Printf.sprintf "oniguruma %S unparseable: %s" oni e.msg) false
       | Ok back -> check (Printf.sprintf "oniguruma round trip %S" oni) (equivalent r back))
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
  check "supplementary in a range"
    (matches (range ~lo:0x10000 ~hi:0x10FFFF) deseret);
  check "round trips through source"
    (match of_string (to_string (singleton 0x10400)) with
     | Ok r -> matches r deseret
     | Error _ -> false)
;;

(* -- the DFA accepts what the regexes accept ------------------------------- *)

module Dfa = Redfa.Dfa

(* Traverse the DFA over [s], returning the accepts list at the state it
   lands in, or [] if it gets stuck. Stuck means every item's
   derivative died, so no token can match, matching the empty accepts
   list the reference gives. *)
let dfa_accepts dfa s =
  let rec go id i =
    if i >= String.length s
    then Dfa.accepts dfa id
    else (
      let cp = Char.code s.[i] in
      match List.find_opt (fun (cs, _) -> Ucharset.mem cs cp) (Dfa.transitions dfa id) with
      | None -> []
      | Some (_, dst) -> go dst (i + 1))
  in
  go (Dfa.initial dfa) 0
;;

let () =
  let st = Random.State.make [| 99 |] in
  for _ = 1 to 400 do
    let tokens = List.init (1 + Random.State.int st 4) (fun i -> i, fst (gen st 3)) in
    let dfa = Dfa.of_tokens tokens in
    let mini = Dfa.minimise dfa in
    List.iter
      (fun s ->
         let expected =
           List.filter_map
             (fun (cid, r) -> if Ast.eval (to_ast r) s then Some cid else None)
             tokens
         in
         check "dfa accepts" (dfa_accepts dfa s = expected);
         check "minimised dfa accepts" (dfa_accepts mini s = expected))
      corpus;
    (* [reaches] is what is still possible from a state, so it holds
       everything [accepts] does, and a state with nothing reachable
       has nowhere to go. *)
    for id = 0 to Dfa.num_states dfa - 1 do
      let acc = Dfa.accepts dfa id
      and rch = Dfa.reaches dfa id in
      check "reaches covers accepts" (List.for_all (fun c -> List.mem c rch) acc);
      check "reaches ascending and distinct" (rch = List.sort_uniq Int.compare rch);
      check "accepts ascending and distinct" (acc = List.sort_uniq Int.compare acc);
      check
        "is_dead agrees with accepts and transitions"
        (Dfa.is_dead dfa id = (acc = [] && Dfa.transitions dfa id = []))
    done;
    (* Transitions out of a state are pairwise disjoint, which is what
       makes the traversal above deterministic. *)
    for id = 0 to Dfa.num_states dfa - 1 do
      let css = List.map fst (Dfa.transitions dfa id) in
      let rec disjoint = function
        | [] | [ _ ] -> true
        | x :: rest ->
          List.for_all (fun y -> Ucharset.is_empty (Ucharset.inter x y)) rest && disjoint rest
      in
      check "transitions disjoint" (disjoint css);
      check "no empty transition label" (not (List.exists Ucharset.is_empty css))
    done
  done
;;

let () =
  if !failures = 0
  then print_endline "all property checks passed"
  else (
    Printf.printf "%d failures\n" !failures;
    exit 1)
;;
