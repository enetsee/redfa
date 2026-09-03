(* -- redfa --------------------------------------------------------------------

   A regex engine over the Unicode codespace, with the boolean
   operations (intersection, complement) alongside the usual ones, and
   DFA construction by Brzozowski derivative.

   {!Regex} is where a caller builds or parses a regex. {!Dfa} turns a
   list of them into an automaton. {!Ast} is the normal form the
   engine derives over, exposed for tests and benchmarks.

   {2 Domains}

   Single-domain. The hash-cons table behind {!Ast} is global and
   unsynchronised, as are the memo fields on every node, so two domains
   calling into this library at once race — on the table, on the tag
   counter, and on the memos. Nothing here is safe to share across
   domains, values included: a node built on one domain may be
   structurally merged with one built on another only by accident.
   Confine a program's use of redfa to one domain, or guard it with a
   lock of your own.
   -------------------------------------------------------------------------- *)

module Ast : sig
  (* -- normal form ------------------------------------------------------------

     The hash-consed regex the derivative engine works over. Smart
     constructors flatten nested [Seq]/[Alt]/[Inter] and sort [Alt] and
     [Inter] children by tag, dropping duplicates (associativity,
     commutativity, idempotence). Regexes differing only by those share
     a node, so [equal] is pointer equality, and deriving reaches a
     finite set of terms, which is what makes DFA construction
     terminate.

     [t] is abstract. It carries a tag and memo slots that serve [deriv]
     and [approx_partition]. Nothing here is stable.
     ------------------------------------------------------------------------ *)

  type t

  (* [tag] is the node's identity in the intern table, handed out in
     allocation order, and [compare] and [hash] are built from it. So
     the order is the order the nodes were first interned in, says
     nothing about their structure, and does not survive a run: a node
     collected and interned again ranks differently against nodes
     interned before it. Fit for a [Map] or [Set] key within a run,
     not for one to store or sort by. [equal] is pointer equality. *)

  val tag : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  (* -- the intern table ------------------------------------------------------

     Nodes are held weakly and collected once nothing outside the table
     refers to them, and the bucket array is resized from the count of
     entries still live, so it shrinks as readily as it grows. It is
     re-measured only when something is interned, though: a program that
     builds a large automaton, drops it and then stops interning holds
     the array at its peak until it interns again, or until it calls
     {!clear_cache}.

     [clear_cache ()] empties the table and returns the array to its
     initial size. Interning starts over, so a node from before the call
     and one from after are distinct records even when structurally
     equal: {!equal} answers false for such a pair, and the sorted-
     distinct form of [Alt] and [Inter], which is maintained by tag, is
     only guaranteed among nodes interned between the same two clears.
     Tags stay unique for the life of the process either way. Call it
     only once every {!Regex.t} and {!Dfa.t} built so far has been
     dropped.
     ------------------------------------------------------------------------ *)

  val clear_cache : unit -> unit

  (* -- constants and predicates -------------------------------------------- *)

  val empty : t
  val eps : t
  val any : t
  val is_empty : t -> bool
  val is_eps : t -> bool
  val is_chars : t -> bool
  val is_nullable : t -> bool

  (* [eps] is the canonical [Seq] of nothing, so [is_seq eps] holds. *)
  val is_seq : t -> bool
  val is_alt : t -> bool

  (* -- sorted-distinct list helpers (sorted by tag) ------------------------ *)

  val sort_distinct : t list -> t list

  (* -- smart constructors -------------------------------------------------- *)

  (* {!singleton} and {!range} validate their codepoints through
     Ucharset, so a surrogate or a value outside [0 .. 0x10FFFF]
     raises [Invalid_argument]. *)
  val chars : Ucharset.t -> t
  val singleton : int -> t
  val range : lo:int -> hi:int -> t

  (* The codepoints of [s], decoded as UTF-8, in sequence. Raises
     [Invalid_argument] on malformed UTF-8, which is the input
     {!Regex.of_string} answers [Error] on; taking it would decode the
     bad bytes to U+FFFD and denote a term the caller did not
     write. *)
  val str : string -> t
  val seq : t -> t -> t
  val seqs : t list -> t
  val alt : t -> t -> t
  val alts : t list -> t
  val inter : t -> t -> t
  val inters : t list -> t
  val complement : t -> t
  val star : t -> t
  val plus : t -> t
  val opt : t -> t

  (* Children of a [Seq] or an [Alt], or the node in a singleton list.
     [seq_children eps] is the empty list, [eps] being the [Seq] of
     nothing, not a list holding [eps]. *)
  val seq_children : t -> t list
  val alt_children : t -> t list

  (* -- derivative ---------------------------------------------------------- *)

  val first_set : t -> Ucharset.t
  val deriv : t -> uchr:int -> t

  (* Whether [s] is in the language, by folding [deriv] over its
     codepoints. *)
  val eval : t -> string -> bool

  (* -- approximate charset partition --------------------------------------- *)

  val approx_partition : t -> Ucharset.Partition.t
  val approx_representatives : t -> int list
  val approx_charset : t -> Ucharset.t list

  (* -- deciding a language ----------------------------------------------------

     Emptiness and equivalence of the language, not of the term:
     {!equal} separates [a*a*] from [a*], and {!is_empty} misses
     [a & ~a]. Both are exact, over the whole codespace and every
     construct the type carries.

     A node and {!deriv} are a deterministic automaton, so both
     traverse one instead of building it: {!is_empty_language} looks
     for a nullable derivative, {!equivalent} is Hopcroft & Karp over
     pairs of them. Neither is bounded or interruptible; deciding
     costs about what {!Dfa.of_tokens} costs, and [.*a.{20}] is two
     million states.
     ------------------------------------------------------------------------ *)

  (* Whether no string at all matches. *)
  val is_empty_language : t -> bool

  (* Whether the two denote the same language. *)
  val equivalent : t -> t -> bool

  (* The same two, given up on past [max_states] states of the
     automaton being traversed: the derivatives visited for
     {!is_empty_language_within}, the pairs merged for
     {!equivalent_within}. [None] is "no answer within that" and not
     an answer. Pass a bound on anything a caller did not write.

     A bound of zero can still answer, where nothing has to be
     traversed to know: [is_empty_language_within ~max_states:0 eps]
     is [Some false], the root being nullable. *)
  val is_empty_language_within : max_states:int -> t -> bool option
  val equivalent_within : max_states:int -> t -> t -> bool option

  (* -- pretty-printing ----------------------------------------------------- *)

  val pp : Format.formatter -> t -> unit
end

module Regex : sig
  (* -- surface syntax ---------------------------------------------------------

     The regex users write, keeping the shape they wrote it in. [Plus],
     [Opt] and [Neg_chars] are constructors of their own, so a regex
     survives the round trip to source with its [+], [?] and [\[^...\]]
     intact.

     {!to_ast} lowers to the normal form the derivative engine works
     over, which is where {!Dfa} takes it.
     ------------------------------------------------------------------------ *)

  type t =
    | Chars of Ucharset.t
    | Neg_chars of Ucharset.t (* any single codepoint outside the set *)
    | Eps
    | Seq of t list
    | Alt of t list
    | Star of t
    | Plus of t
    | Opt of t
    | Complement of t (* of the language, see {!complement} *)
    | Inter of t list

  (* -- constants and predicates -------------------------------------------- *)

  val empty : t
  val eps : t
  val any : t
  val is_empty : t -> bool
  val is_eps : t -> bool
  val is_nullable : t -> bool

  (* The set a [Chars] or [Neg_chars] denotes, [None] for anything
     else. *)
  val charset_of : t -> Ucharset.t option

  (* -- constructors -----------------------------------------------------------

     Smart constructors, taking the local algebraic simplifications and
     leaving the rest as written.

     Every one taking a raw [int] codepoint validates it through
     Ucharset, so a surrogate or a value outside [0 .. 0x10FFFF]
     raises [Invalid_argument]. The [_char] and [_uchar] forms take
     scalar values already and cannot raise.
     ------------------------------------------------------------------------ *)

  val chars : Ucharset.t -> t
  val singleton : int -> t
  val singleton_char : char -> t
  val singleton_uchar : Uchar.t -> t
  val range : lo:int -> hi:int -> t
  val range_char : lo:char -> hi:char -> t
  val range_uchar : lo:Uchar.t -> hi:Uchar.t -> t

  (* Any single codepoint outside the set, the [\[^...\]] of a
     source. *)
  val not_chars : Ucharset.t -> t
  val not_singleton : int -> t
  val not_singleton_char : char -> t
  val not_singleton_uchar : Uchar.t -> t
  val not_range : lo:int -> hi:int -> t
  val not_range_char : lo:char -> hi:char -> t
  val not_range_uchar : lo:Uchar.t -> hi:Uchar.t -> t

  (* Any of the listed codepoints. *)
  val chars_of_list : int list -> t
  val chars_of_char_list : char list -> t
  val chars_of_uchar_list : Uchar.t list -> t

  (* Any codepoint inside any of the inclusive ranges. *)
  val chars_in_ranges : (int * int) list -> t
  val chars_in_char_ranges : (char * char) list -> t
  val chars_in_uchar_ranges : (Uchar.t * Uchar.t) list -> t

  (* Any of the listed codepoints, or any codepoint inside any of the
     ranges. The trailing [unit] guards against a partial application
     when both labels are left off. *)
  val one_of : ?singles:int list -> ?ranges:(int * int) list -> unit -> t
  val one_of_char : ?singles:char list -> ?ranges:(char * char) list -> unit -> t

  val one_of_uchar
    :  ?singles:Uchar.t list
    -> ?ranges:(Uchar.t * Uchar.t) list
    -> unit
    -> t

  (* The codepoints of [s], decoded as UTF-8, in sequence. Raises
     [Invalid_argument] on malformed UTF-8, which is the input
     {!of_string} answers [Error] on; taking it would decode the bad
     bytes to U+FFFD and denote a term the caller did not write. *)
  val str : string -> t
  val seq : t -> t -> t
  val seqs : t list -> t
  val alt : t -> t -> t
  val alts : t list -> t
  val star : t -> t
  val plus : t -> t
  val opt : t -> t

  (* The complement of the language: every string the argument does not
     match, which takes in the empty string and strings of any length.
     [complement (singleton_char 'a')] matches [""], ["b"] and ["ab"].

     A negated character class is {!not_chars} and its friends, or
     [\[^a\]] in a source. *)
  val complement : t -> t
  val inter : t -> t -> t
  val inters : t list -> t

  (* -- parsing ----------------------------------------------------------------

       alt    := inter ('|' inter)*
       inter  := concat ('&' concat)*
       concat := prefix*
       prefix := '~' prefix | repeat
       repeat := atom ('*' | '+' | '?')*
       atom   := '(' alt ')' | '[' class ']' | '.' | escape | literal

     A postfix binds tighter than the [~] prefix: [~a*] complements
     [a*] rather than repeating [~a], so it does not match the empty
     string. Write [(~a)*] for the other reading. And [~] takes only
     the one repeat that follows it, so [~ab] is [(~a)b], not
     [~(ab)].

     Escapes are [\t], [\n], [\r], [\f], [\0], [\u{HHHH}], the shorthand
     classes [\d], [\w], [\s] with their negations, and a backslash
     before any printable ASCII character that is not a letter or a
     digit, for that character literally. Space, the C0 controls and
     DEL are not metacharacters, so a backslash before one is an
     error rather than that character.
     ------------------------------------------------------------------------ *)

  type error =
    { pos : int (* byte offset into the source *)
    ; msg : string
    }

  val of_string : string -> (t, error) result

  (* The source with a caret under the offending byte. *)
  val error_to_string : string -> error -> string

  (* -- lowering and comparison --------------------------------------------- *)

  (* The source form {!of_string} reads back. Total over the type, the
     constructors being public: [Alt \[\]] goes out as the empty
     language and [Inter \[\]] as [.*], which is what {!to_ast} makes
     of them. *)
  val to_string : t -> string
  val to_ast : t -> Ast.t

  (* -- deciding a language ----------------------------------------------------

     Exact, over the whole codespace and every construct the type
     carries. Both lower through {!to_ast} and hand the question to
     {!Ast}, which carries the algorithm and the cost. The cost is
     unbounded.
     ------------------------------------------------------------------------ *)

  (* Whether no string at all matches. Not {!is_empty}, which asks
     about the term: [a&~a] and [a.*&b.*] are empty languages it says
     nothing about. *)
  val is_empty_language : t -> bool

  (* Whether the two denote the same language. Exact, so [a*a*] is
     equivalent to [a*], and [(ab)*a] to [a(ba)*].

     This used to be equality of the lowered terms, which is
     equivalence up to associativity, commutativity and idempotence
     only. That test is still [Ast.equal (to_ast a) (to_ast b)], and
     is what a {!to_string} round trip should be checked against: an
     equivalent term back is weaker than the same term back. *)
  val equivalent : t -> t -> bool

  (* The same two under a state budget; see {!Ast.equivalent_within}.
     [None] is "no answer within that" and not an answer. *)
  val is_empty_language_within : max_states:int -> t -> bool option
  val equivalent_within : max_states:int -> t -> t -> bool option

  (* -- emission ------------------------------------------------------------ *)

  (* Oniguruma source. [Error] where the term has no Oniguruma form: a
     [Complement], an [Inter] over anything but charsets, or the empty
     language. *)
  val to_oniguruma : t -> (string, string) result

  (* -- pretty-printing --------------------------------------------------------

     A debug view, not source: character sets print as {!Ucharset.pp}
     writes them, [Eps] as an epsilon, an empty [Alt] and an empty
     [Inter] as their languages. A [Neg_chars] takes a [^] prefix and a
     [Complement] a [~], so the two negations are told apart, and the
     [^] parenthesises like the [~]. Use {!to_string} for source.
     ------------------------------------------------------------------------ *)

  val pp : Format.formatter -> t -> unit
end

module Dfa : sig
  (* -- deterministic finite automaton -----------------------------------------

     Built from a list of token regexes by item-set derivative
     construction. A state is a set of items, one per candidate token,
     each carrying the suffix of its regex still to match. Transitions
     come from deriving every item on a representative codepoint of
     each block of the joint approximate partition.

     The normal form quotients terms by associativity, commutativity
     and idempotence, leaving finitely many derivatives, so
     construction terminates.
     ------------------------------------------------------------------------ *)

  type state_id = int
  type t

  (* [of_tokens [(c0, r0); (c1, r1); ...]] builds a DFA whose initial
     state holds one item per pair. State 0 is the initial state.

     Case ids come back from {!accepts} in ascending order, so a caller
     wanting declaration order to be priority order should number
     tokens that way (lower id, higher priority).

     Two pairs may share a case id with different regexes. The items
     evolve separately under derivation, while {!accepts} and
     {!reaches} report each token at most once. *)
  val of_tokens : (int * Regex.t) list -> t

  (* The same, [None] if the automaton would hold more than
     [max_states] states. Construction is the operation here whose
     cost a caller cannot see coming: [.*a.{20}] is two million
     states, [.*a.{12}] is 8192, and a tower of complements and
     intersections is non-elementary.
     Pass a bound on anything a caller did not write. A bound below
     one always gives [None], an automaton needing an initial state. *)
  val of_tokens_within : max_states:int -> (int * Regex.t) list -> t option

  (* Always 0, for symmetry with {!num_states}. *)
  val initial : t -> state_id
  val num_states : t -> int

  (* Case ids whose regex is nullable in this state, in ascending
     order: the tokens this state accepts. *)
  val accepts : t -> state_id -> int list

  (* Case ids still present in this state's item set. A superset of
     {!accepts}, and an over-approximation of what can still match: an
     item is dropped only when its regex derives to [empty], and a
     regex can denote the empty language without the normal form
     saying so. {!minimise} does not tighten it, and the states
     concerned need not be dead -- the tokens "a(b.*&c.*)" and "a"
     give an automaton whose every state lists both, before and after,
     though only the second can ever match. *)
  val reaches : t -> state_id -> int list

  (* Outgoing transitions. Each [(charset, dest)] means any codepoint in
     [charset] goes to [dest]. The charsets are pairwise disjoint, and
     arrive in ascending order of least codepoint. A codepoint in none
     of them has no transition. *)
  val transitions : t -> state_id -> (Ucharset.t * state_id) list

  (* Accepts nothing and goes nowhere. *)
  val is_dead : t -> state_id -> bool
  val iter_states : t -> (state_id -> unit) -> unit

  (* The smallest DFA accepting the same tokens. Two things happen:
     states that accept the same tokens and, on every input, go to
     equivalent states are merged, and states no accepting state is
     reachable from are dropped along with every edge into them.

     Construction already quotients terms by associativity,
     commutativity and idempotence, so a merge is two terms denoting
     the same language through different structure. A drop is a term
     whose language is empty without the normal form noticing, such as
     the intersection left after deriving "a(b.*&c.*)" on [a].

     The initial state of the result is the one holding the original
     initial state, and the rest are numbered by breadth-first search
     from it, so the numbering is canonical for a given input.
     {!reaches} at a merged state is the union of those merged; a
     dropped state takes its own with it. {!is_dead} is false of every
     state of the result, the one exception being the automaton for
     the empty language, which is a single dead state because an
     automaton still needs an initial one. Idempotent. *)
  val minimise : t -> t
  val pp : Format.formatter -> t -> unit
end
