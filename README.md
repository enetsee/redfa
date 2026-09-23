# redfa

[![CI](https://github.com/enetsee/redfa/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/enetsee/redfa/actions/workflows/ci.yml)
[![Docs](https://github.com/enetsee/redfa/actions/workflows/docs.yml/badge.svg?branch=main)](https://github.com/enetsee/redfa/actions/workflows/docs.yml)

A regex engine over the Unicode codespace, built on Brzozowski derivatives, with
intersection and complement as well as the usual operations.

Intersection and complement are the main feature. With derivatives they cost
no more to implement than concatenation, and they let you write "an identifier
that is not a keyword" as one regex and build a DFA from it.

The engine follows Owens, Reppy and Turon, [*Regular-expression derivatives
re-examined*][owens09] (Journal of Functional Programming 19(2):173-190, 2009).
Three ideas come from that paper:

- The derivative of an intersection or complement is defined as simply as that
  of any other operator.
- Identifying terms that differ only by associativity, commutativity and
  idempotence makes the set of reachable derivatives finite.
- Computing one transition per *derivative class* (a set of characters that
  all give the same derivative) instead of one per character makes an alphabet
  the size of Unicode practical.

`Ast` implements the normal form from the second point, and `Dfa` partitions the
alphabet into the classes from the third.

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
  (* These two match the same strings, checked exactly. *)
  assert (Regex.equivalent (parse "(ab)*a") (parse "a(ba)*"));
  (* This one matches no string. *)
  assert (Regex.is_empty_language (parse "a.*&b.*"))
```

## What is here

**`Regex`** is the syntax users write: a parser, a printer whose output parses
back to the same term, Oniguruma emission, and constructors that keep the
structure as written. The grammar adds `&` for intersection and `~` for
complement to the usual syntax.

**`Ast`** is the hash-consed normal form the engine takes derivatives of. Terms
that differ only by associativity, commutativity and idempotence are the same
node, so equality is a pointer comparison, and a term has finitely many
derivatives, so DFA construction terminates.

**`Dfa`** builds an automaton from a list of token regexes by item-set
derivative construction, and minimises it to the Myhill–Nerode minimum. For a
code generator it also provides the character classes and a transition table
indexed by state and class. This is usually far smaller than a separate
dispatch per state: a lexer with two hundred keywords has 814 states and 34
classes.

**Deciding languages.** `equivalent` and `is_empty_language` give exact
answers, over the whole codespace and every construct in the type, using
Hopcroft–Karp over derivatives. So `a*a*` is equivalent to `a*`, and
`a.*&b.*` is empty although its normal form is an intersection of two
nonempty terms.

**Bounds.** DFA construction and both decisions can take time and memory
exponential in the size of the regex: `.*a.{20}` has two million states.
`of_tokens_within`, `equivalent_within` and `is_empty_language_within` take a
state budget and return `None` when it is exceeded. Use them on patterns from
untrusted input.

Single-domain: the intern table and the memos on every node are unsynchronised.

## Development

```sh
dune build
dune runtest
dune exec --profile release bench/dfa_bench.exe
```

## License

MIT. See [LICENSE](LICENSE).
