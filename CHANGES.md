## Unreleased

First release. A regex engine over the Unicode codespace built on Brzozowski
derivatives, with intersection and complement alongside the usual operations:

- `Regex`, the surface syntax, with a parser, a printer that round-trips, and
  Oniguruma emission.
- `Ast`, the hash-consed normal form the engine derives over.
- `Dfa`, item-set derivative construction and minimisation to the
  Myhill-Nerode minimum, plus the character classes and transition table a code
  generator emits against.
- `equivalent` and `is_empty_language`, exact over the whole codespace, by
  Hopcroft-Karp over derivatives.
- State budgets on construction and both decisions, for patterns a caller did
  not write.
