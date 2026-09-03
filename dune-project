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
  "Brzozowski derivatives over the Unicode codespace, with the boolean
operations alongside the usual ones. Regexes are hash-consed into a normal form
that quotients associativity, commutativity and idempotence, which is what
leaves finitely many derivatives and makes DFA construction terminate. On top
of that: minimisation to the Myhill-Nerode minimum, exact decisions for
emptiness and language equivalence by Hopcroft-Karp over derivatives, state
budgets so an untrusted pattern cannot run away with the machine, and the
character classes and transition table a code generator emits against.")
 (depends
  (ocaml
   (>= 4.14))
  (ucharset
   (>= 0.2.0)))
 (tags
  (regex dfa derivative unicode lexer intersection complement)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
