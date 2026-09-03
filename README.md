# redfa

[![CI](https://github.com/enetsee/redfa/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/enetsee/redfa/actions/workflows/ci.yml)
[![Docs](https://github.com/enetsee/redfa/actions/workflows/docs.yml/badge.svg?branch=main)](https://github.com/enetsee/redfa/actions/workflows/docs.yml)

A regex engine over the Unicode codespace, built on Brzozowski derivatives, with
the boolean operations alongside the usual ones.

Intersection and complement are the point. A derivative-based engine gets them
for the same price as concatenation, and they are what let you say "an
identifier that is not a keyword" as one regex and hand it to a DFA.

The engine follows Owens, Reppy and Turon, [*Regular-expression derivatives
re-examined*][owens09] (Journal of Functional Programming 19(2):173-190, 2009),
which is where the three things it rests on come from: derivatives give the
boolean operations for nothing, quotienting terms by associativity,
commutativity and idempotence keeps the reachable set finite, and taking a
transition on one representative per *derivative class* rather than per
character is what makes an alphabet the size of Unicode tractable at all.
`Ast` is that normal form, and `Dfa`'s partition is those classes.

[owens09]: https://doi.org/10.1017/S0956796808007090

[API documentation](https://enetsee.github.io/redfa/redfa/Redfa/index.html)

## Install

```sh
opam install redfa
```

The development version:

```sh
opam pin add redfa https://github.com/enetsee/redfa.git
```

64-bit only, inherited from [ucharset](https://github.com/enetsee/ucharset),
which packs interval endpoints two to an `int`.

## Example

```ocaml
open Redfa

let parse src =
  match Regex.of_string src with
  | Ok r -> r
  | Error e -> failwith (Regex.error_to_string src e)

let ident = parse "[a-z][a-z0-9_]*"
let keyword = parse "let|in|fun"

(* An identifier that is not a keyword. *)
let other = Regex.inter ident (Regex.complement keyword)

let () =
  assert (Ast.eval (Regex.to_ast other) "letter");
  assert (not (Ast.eval (Regex.to_ast other) "let"));
  (* Decided, not guessed: these two denote the same language. *)
  assert (Regex.equivalent (parse "(ab)*a") (parse "a(ba)*"));
  (* And this one denotes none at all. *)
  assert (Regex.is_empty_language (parse "a.*&b.*"))
```

## What is here

**`Regex`** is the surface: a parser, a printer that round-trips, Oniguruma
emission, and constructors that keep the shape you wrote. The grammar adds `&`
for intersection and `~` for complement to the usual syntax.

**`Ast`** is the hash-consed normal form the engine derives over. It quotients
associativity, commutativity and idempotence, so equal terms share a node,
equality is a pointer comparison, and deriving reaches finitely many terms —
which is what makes the construction below terminate.

**`Dfa`** builds an automaton from a list of token regexes by item-set
derivative construction, and minimises it to the Myhill–Nerode minimum. For a
code generator it also hands over the character classes and the transition
table, which is usually far smaller than a dispatch per state: a lexer with two
hundred keywords is 814 states and 34 classes.

**Deciding languages.** `equivalent` and `is_empty_language` are exact, over the
whole codespace and every construct in the type, by Hopcroft–Karp over
derivatives. So `a*a*` is equivalent to `a*`, and `a & ~a` is recognised as
empty although the normal form leaves it a live term.

**Bounds.** Construction and both decisions are unbounded by nature — a regex
can denote an automaton larger than the machine. `of_tokens_within`,
`equivalent_within` and `is_empty_language_within` take a state budget and
answer `None` rather than running away, which is what to use on a pattern a
caller did not write.

Single-domain: the intern table and the memos on every node are unsynchronised.

## Development

```sh
dune build
dune runtest
dune exec --profile release bench/dfa_bench.exe
```

## License

MIT. See [LICENSE](LICENSE).
