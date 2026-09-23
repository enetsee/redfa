## Unreleased

First release. A regex engine over the Unicode codespace built on Brzozowski
derivatives, with intersection and complement as well as the usual operations:

- `Regex`, the syntax users write, with a parser, a printer whose output
  parses back to the same term, and Oniguruma emission.
- `Ast`, the hash-consed normal form the engine takes derivatives of.
- `Dfa`, item-set derivative construction and minimisation to the
  Myhill-Nerode minimum, plus the character classes and transition table for a
  code generator to emit.
- `equivalent` and `is_empty_language`, exact over the whole codespace, by
  Hopcroft-Karp over derivatives.
- State budgets on construction and both decisions, for patterns from
  untrusted input.
