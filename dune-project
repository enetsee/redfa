(lang dune 3.20)

(name redfa)

(generate_opam_files true)

(source
 (github enetsee/redfa))

(authors "Michael Thomas <mthomas180@gmail.com>")

(maintainers "Michael Thomas <mthomas180@gmail.com>")

(license MIT)

(documentation https://enetsee.github.io/redfa/)

(package
 (name redfa)
 (synopsis "A derivative-based regex engine with intersection and complement")
 (description
  "Brzozowski derivatives over the Unicode codespace, with intersection and
complement as well as the usual operations. Regexes are hash-consed into a
normal form that identifies terms differing only by associativity,
commutativity and idempotence, so every regex has finitely many derivatives
and DFA construction terminates. Also: minimisation to the Myhill-Nerode
minimum, exact emptiness and equivalence checks by Hopcroft-Karp over
derivatives, state budgets that bound the work done on untrusted patterns, and
the character classes and transition table for a code generator to emit.")
 (depends
  (ocaml
   (>= 4.14))
  (ucharset
   (>= 0.2.0)))
 (tags
  (regex dfa derivative unicode lexer intersection complement)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
