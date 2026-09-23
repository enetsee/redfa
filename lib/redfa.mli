(** A regex engine over the Unicode codespace, with the boolean
    operations (intersection, complement) alongside the usual ones, and
    DFA construction by Brzozowski derivative.

    {!Regex} is where a caller builds or parses a regex. {!Dfa} turns a
    list of them into an automaton. {!Ast} is the normal form the
    engine derives over, exposed for tests and benchmarks.

    {2 Domains}

    Single-domain. The hash-cons table behind {!Ast} is global and
    unsynchronised, as are the memo fields on every node, so two domains
    calling into this library at once race — on the table, on the tag
    counter, and on the memos. Values are unsafe to share across
    domains too: two structurally equal nodes built on different
    domains may or may not end up as the same record, so {!Ast.equal}
    between them is unreliable. Confine a program's use of redfa to
    one domain, or guard it with a lock of your own. *)

module Ast : sig
  (** The hash-consed regex the derivative engine works over. Smart
      constructors flatten nested [Seq]/[Alt]/[Inter] and sort [Alt] and
      [Inter] children by tag, dropping duplicates (associativity,
      commutativity, idempotence). Regexes differing only by those share
      a node, so [equal] is pointer equality, and deriving reaches a
      finite set of terms, which is what makes DFA construction
      terminate.

      An [Inter] with a [Chars] child matches single codepoints only,
      so its other children are intersected into that charset where
      possible: [inter any (complement (chars s))] becomes
      [chars (comp s)], and [a & ~a] becomes the empty term.

      [t] is abstract. Each node has a tag and memo fields used by
      [deriv] and [approx_partition]. This module's interface may change
      without notice. *)

  type t

  (** [tag] is the node's identity in the intern table, assigned in
      allocation order, and [compare] and [hash] are built from it. The
      order is therefore the order the nodes were first interned in,
      and it is valid only within one run; a node collected and
      interned again gets a new tag, which orders it after every node
      interned before it. Use it as a [Map] or [Set] key within a run,
      and compare by structure where the order has to be the same
      across runs. [equal] is pointer equality. *)

  val tag : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  (** {2 The intern table}

      Nodes are held weakly and collected once nothing outside the table
      refers to them, and the bucket array is resized from the count of
      entries still live, so it shrinks as well as grows. The size is
      checked only when a node is interned: if a program builds a large
      automaton, drops it and then stops interning, the array stays at
      its peak size until the next intern or a call to {!clear_cache}.

      [clear_cache ()] empties the table and returns the array to its
      initial size. Interning starts over, so a node from before the call
      and one from after are distinct records even when structurally
      equal: {!equal} returns false for such a pair, and the sorted-
      distinct form of [Alt] and [Inter], which is maintained by tag, is
      only guaranteed among nodes interned between the same two clears.
      Tags stay unique for the life of the process either way. Call it
      only once every {!Regex.t} and {!Dfa.t} built so far has been
      dropped. *)

  val clear_cache : unit -> unit

  (** {2 Constants and predicates} *)

  val empty : t
  val eps : t
  val any : t
  val is_empty : t -> bool
  val is_eps : t -> bool
  val is_chars : t -> bool
  val is_nullable : t -> bool

  (** [eps] is the canonical [Seq] of nothing, so [is_seq eps] holds. *)
  val is_seq : t -> bool

  val is_alt : t -> bool

  (** {2 Sorted-distinct list helpers (sorted by tag)} *)

  val sort_distinct : t list -> t list

  (** {2 Smart constructors} *)

  (** {!singleton} and {!range} validate their codepoints through
      Ucharset, so a surrogate or a value outside [0 .. 0x10FFFF]
      raises [Invalid_argument]. *)

  val chars : Ucharset.t -> t
  val singleton : int -> t
  val range : lo:int -> hi:int -> t

  (** The codepoints of [s], decoded as UTF-8, in sequence. Raises
      [Invalid_argument] on malformed UTF-8, the same input on which
      {!Regex.of_string} returns [Error]. Accepting it would decode the
      bad bytes to U+FFFD and build a term that differs from the
      input. *)
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

  (** Children of a [Seq] or an [Alt], or the node in a singleton list.
      [seq_children eps] is the empty list, [eps] being the [Seq] of
      nothing. *)

  val seq_children : t -> t list
  val alt_children : t -> t list

  (** {2 Derivative} *)

  val first_set : t -> Ucharset.t
  val deriv : t -> uchr:int -> t

  (** Whether [s] is in the language, by folding [deriv] over its
      codepoints.

      Raises [Invalid_argument] on malformed UTF-8, as {!str} does and
      for the same reason: the bad bytes would otherwise decode to
      U+FFFD and be matched as that codepoint, so a regex for U+FFFD
      would match each of the 128 lone bytes [0x80 .. 0xFF]. The whole
      string is validated before matching starts, so whether it raises
      does not depend on the term. Check with
      [String.is_valid_utf_8] to handle the case rather than catch
      it. *)
  val eval : t -> string -> bool

  (** {2 Approximate charset partition} *)

  val approx_partition : t -> Ucharset.Partition.t
  val approx_representatives : t -> int list
  val approx_charset : t -> Ucharset.t list

  (** {2 Deciding a language}

      Emptiness and equivalence of the language. {!equal} compares
      terms, so it returns false for [a*a*] and [a*]; {!is_empty} tests
      whether the node is the [empty] term, so it returns false for
      [a & aa], which matches nothing. The functions below are exact,
      over the whole codespace and every construct the type has.

      A term and its derivatives under {!deriv} form a deterministic
      automaton, and both functions explore it state by state without
      building a {!Dfa.t}. {!is_empty_language} searches for a nullable
      derivative; {!equivalent} is Hopcroft & Karp over pairs of them.
      Neither has a limit on the work done, and each costs about what
      {!Dfa.of_tokens} costs on the same input ([.*a.{20}] is two
      million states). *)

  (** Whether no string at all matches. *)
  val is_empty_language : t -> bool

  (** Whether the two denote the same language. *)
  val equivalent : t -> t -> bool

  (** The same two, returning [None] once the traversal exceeds
      [max_states] states: the derivatives visited for
      {!is_empty_language_within}, the pairs merged for
      {!equivalent_within}. Use these on regexes from untrusted input.

      Nullability of the root is tested first, so a bound of zero
      still returns [Some] when that test alone determines the result
      ([is_empty_language_within ~max_states:0 eps] is
      [Some false]). *)

  val is_empty_language_within : max_states:int -> t -> bool option
  val equivalent_within : max_states:int -> t -> t -> bool option

  (** {2 Pretty-printing} *)

  val pp : Format.formatter -> t -> unit
end

module Regex : sig
  (** The regex users write, keeping the shape they wrote it in. [Plus],
      [Opt] and [Neg_chars] are constructors of their own, so a regex
      survives the round trip to source with its [+], [?] and [\[^...\]]
      intact.

      {!to_ast} lowers it to the normal form the derivative engine works
      over, which is the form {!Dfa} builds from. *)

  type t =
    | Chars of Ucharset.t
    | Neg_chars of Ucharset.t (** any single codepoint outside the set *)
    | Eps
    | Seq of t list
    | Alt of t list
    | Star of t
    | Plus of t
    | Opt of t
    | Complement of t (** of the language, see {!complement} *)
    | Inter of t list

  (** {2 Constants and predicates} *)

  val empty : t
  val eps : t
  val any : t
  val is_empty : t -> bool
  val is_eps : t -> bool
  val is_nullable : t -> bool

  (** The set a [Chars] or [Neg_chars] denotes, [None] for anything
      else. *)
  val charset_of : t -> Ucharset.t option

  (** {2 Constructors}

      Smart constructors. They apply local algebraic simplifications
      and otherwise keep the structure as written.

      Every one taking a raw [int] codepoint validates it through
      Ucharset, so a surrogate or a value outside [0 .. 0x10FFFF]
      raises [Invalid_argument]. The [_char] and [_uchar] forms take
      scalar values already, so they always succeed. *)

  val chars : Ucharset.t -> t
  val singleton : int -> t
  val singleton_char : char -> t
  val singleton_uchar : Uchar.t -> t
  val range : lo:int -> hi:int -> t
  val range_char : lo:char -> hi:char -> t
  val range_uchar : lo:Uchar.t -> hi:Uchar.t -> t

  (** Any single codepoint outside the set, the [\[^...\]] of a
      source. *)

  val not_chars : Ucharset.t -> t
  val not_singleton : int -> t
  val not_singleton_char : char -> t
  val not_singleton_uchar : Uchar.t -> t
  val not_range : lo:int -> hi:int -> t
  val not_range_char : lo:char -> hi:char -> t
  val not_range_uchar : lo:Uchar.t -> hi:Uchar.t -> t

  (** Any of the listed codepoints. *)

  val chars_of_list : int list -> t
  val chars_of_char_list : char list -> t
  val chars_of_uchar_list : Uchar.t list -> t

  (** Any codepoint inside any of the inclusive ranges. *)

  val chars_in_ranges : (int * int) list -> t
  val chars_in_char_ranges : (char * char) list -> t
  val chars_in_uchar_ranges : (Uchar.t * Uchar.t) list -> t

  (** Any of the listed codepoints, or any codepoint inside any of the
      ranges. The trailing [unit] guards against a partial application
      when both labels are left off. *)

  val one_of : ?singles:int list -> ?ranges:(int * int) list -> unit -> t
  val one_of_char : ?singles:char list -> ?ranges:(char * char) list -> unit -> t

  val one_of_uchar
    :  ?singles:Uchar.t list
    -> ?ranges:(Uchar.t * Uchar.t) list
    -> unit
    -> t

  (** The codepoints of [s], decoded as UTF-8, in sequence. Raises
      [Invalid_argument] on malformed UTF-8, the same input on which
      {!of_string} returns [Error]. Accepting it would decode the bad
      bytes to U+FFFD and build a term that differs from the input. *)
  val str : string -> t

  val seq : t -> t -> t
  val seqs : t list -> t
  val alt : t -> t -> t
  val alts : t list -> t
  val star : t -> t
  val plus : t -> t
  val opt : t -> t

  (** The complement of the language: every string the argument does not
      match, including the empty string and strings of every length.
      [complement (singleton_char 'a')] matches [""], ["b"] and ["ab"].

      A negated character class is {!not_chars} and its friends, or
      [\[^a\]] in a source. *)
  val complement : t -> t

  val inter : t -> t -> t
  val inters : t list -> t

  (** {2 Parsing}

      {[
        alt    := inter ('|' inter)*
        inter  := concat ('&' concat)*
        concat := prefix*
        prefix := '~' prefix | repeat
        repeat := atom ('*' | '+' | '?')*
        atom   := '(' alt ')' | '[' class ']' | '.' | escape | literal
      ]}

      A postfix binds tighter than the [~] prefix: [~a*] complements
      [a*] rather than repeating [~a], so it does not match the empty
      string. Write [(~a)*] to repeat [~a]. And [~] applies only to the
      one repeat that follows it, so [~ab] parses as [(~a)b]; write
      [~(ab)] to complement the sequence.

      Escapes are [\t], [\n], [\r], [\f], [\0], [\u{HHHH}], the shorthand
      classes [\d], [\w], [\s] with their negations, and a backslash
      before any printable ASCII character other than a letter or
      digit, which matches that character literally. Space, the C0
      controls and DEL match themselves unescaped, and a backslash
      before one is an error. *)

  type error =
    { pos : int (** byte offset into the source *)
    ; msg : string
    }

  val of_string : string -> (t, error) result

  (** The source with a caret under the offending byte. *)
  val error_to_string : string -> error -> string

  (** {2 Lowering and comparison} *)

  (** Source text that {!of_string} parses back. Defined for every
      value of the type, since the constructors are public: [Alt \[\]]
      is emitted as the empty language and [Inter \[\]] as [.*],
      matching what {!to_ast} lowers them to. *)
  val to_string : t -> string

  val to_ast : t -> Ast.t

  (** {2 Deciding a language}

      Exact, over the whole codespace and every construct the type
      has. Both lower through {!to_ast} and call the {!Ast} functions
      of the same name; see those for the algorithm and its cost. The
      cost has no limit. *)

  (** Whether no string matches. {!is_empty} tests the term instead,
      so it returns false for [a&~a] and [a.*&b.*], which match
      nothing. *)
  val is_empty_language : t -> bool

  (** Whether the two denote the same language. Exact, so [a*a*] is
      equivalent to [a*], and [(ab)*a] to [a(ba)*].

      [Ast.equal (to_ast a) (to_ast b)] is the stricter test,
      equivalence up to associativity, commutativity and idempotence
      only. Check a {!to_string} round trip with that one, since
      getting back an equivalent term is weaker than getting back the
      same one. *)
  val equivalent : t -> t -> bool

  (** The same two under a state budget; see {!Ast.equivalent_within}.
      [None] means "no answer within that budget". *)

  val is_empty_language_within : max_states:int -> t -> bool option
  val equivalent_within : max_states:int -> t -> t -> bool option

  (** {2 Emission} *)

  (** Oniguruma source. [Error] where the term has no Oniguruma form:
      a [Complement], an [Inter] that does not denote a character
      class, or the empty language.

      An [Inter] with a [Chars] or [Neg_chars] child matches single
      codepoints only, so it emits as a character class when its other
      children are charsets or complements of charsets. For example
      [inter any (complement (chars s))] emits as [\[^s\]].

      A class is emitted negated when that has fewer intervals. The
      whole codespace is emitted as [\[\\s\\S\]], which includes
      newline. *)
  val to_oniguruma : t -> (string, string) result

  (** {2 Pretty-printing}

      A debug view, not source: character sets print as [Ucharset.pp]
      writes them, [Eps] as an epsilon, an empty [Alt] and an empty
      [Inter] as their languages. A [Neg_chars] prints with a [^]
      prefix and a [Complement] with [~], so the two negations are
      distinguishable, and [^] is parenthesised the same way as [~].
      Use {!to_string} for source. *)

  val pp : Format.formatter -> t -> unit
end

module Dfa : sig
  (** Built from a list of token regexes by item-set derivative
      construction. A state is a set of items, one per candidate token,
      each being the remainder of its regex still to match. Transitions
      are computed by deriving every item on one representative
      codepoint of each block of the items' joint approximate
      partition.

      The normal form quotients terms by associativity, commutativity
      and idempotence, leaving finitely many derivatives, so
      construction terminates. *)

  type state_id = int
  type t

  (** [of_tokens [(c0, r0); (c1, r1); ...]] builds a DFA whose initial
      state holds one item per pair. State 0 is the initial state.

      {!accepts} returns case ids in ascending order, so to make
      declaration order the priority order, number tokens in
      declaration order (lower id, higher priority).

      Two pairs may share a case id with different regexes. The items
      evolve separately under derivation, while {!accepts} and
      {!reaches} report each token at most once. *)
  val of_tokens : (int * Regex.t) list -> t

  (** The same, [None] if the automaton would hold more than
      [max_states] states. Construction cost is the hardest to predict
      from the input: [.*a.{12}] is 8192 states, [.*a.{20}] is two
      million, and nested complements and intersections can need a
      non-elementary number. Use this on regexes from untrusted input.
      A bound below one always returns [None], since an automaton has
      an initial state. *)
  val of_tokens_within : max_states:int -> (int * Regex.t) list -> t option

  (** Always 0, for symmetry with {!num_states}. *)
  val initial : t -> state_id

  val num_states : t -> int

  (** Case ids whose regex is nullable in this state, in ascending
      order: the tokens this state accepts. *)
  val accepts : t -> state_id -> int list

  (** Case ids still present in this state's item set. A superset of
      {!accepts}, and an over-approximation of what can still match; an
      item is dropped once its regex derives to [empty], and a regex
      can match nothing without its normal form being [empty].
      {!minimise} does not make the list exact, because the extra ids
      also appear on live states, which it keeps. The tokens
      "a(b.*&c.*)" and "a" give an automaton in which every state lists
      both, before and after minimising, though only the second can
      match. *)
  val reaches : t -> state_id -> int list

  (** Outgoing transitions. Each [(charset, dest)] means any codepoint in
      [charset] goes to [dest]. The charsets are pairwise disjoint, and
      are listed in ascending order of least codepoint. A codepoint in
      none of them has no transition. *)
  val transitions : t -> state_id -> (Ucharset.t * state_id) list

  (** Whether the state accepts no token and has no transitions. *)
  val is_dead : t -> state_id -> bool

  val iter_states : t -> (state_id -> unit) -> unit

  (** The smallest DFA accepting the same tokens. Two things happen:
      states that accept the same tokens and, on every input, go to
      equivalent states are merged, and states no accepting state is
      reachable from are dropped along with every edge into them.

      Construction already quotients terms by associativity,
      commutativity and idempotence, so merged states are terms that
      match the same language with different structure. Dropped states
      are terms that match nothing although their normal form is not
      [empty], such as the intersection left after deriving
      "a(b.*&c.*)" on [a].

      The initial state of the result is the one containing the
      original initial state, and the rest are numbered by
      breadth-first search from it, so the numbering is canonical for a
      given input. {!reaches} at a merged state is the union of those
      merged; the case ids of a dropped state are discarded with it.
      {!is_dead} is false of every
      state of the result, the one exception being the automaton for
      the empty language, which is a single dead state because an
      automaton still needs an initial one. Idempotent. *)
  val minimise : t -> t

  (** {2 Emission}

      Views of the automaton for a code generator emitting a lexer.
      {!transitions} is for inspecting one. *)

  (** {!transitions} clipped to [lo .. hi], keeping the entries that
      intersect it. A generator with a fast path over part of the
      codespace (one branch for a single byte of UTF-8, another for the
      rest) can call this once per part, so each branch dispatches only
      on the entries that can match there. With {!transitions} it would
      emit the full dispatch in every branch.

      Empty if [lo > hi]. Raises [Invalid_argument] if either bound is
      outside the codespace or is a surrogate; [Ucharset.range] checks
      the bounds even when the range is empty. A range may span the
      surrogate block, so splitting the codespace by UTF-8 length
      works as is; a caller splitting at arbitrary offsets has to move
      its bounds out of [0xD800 .. 0xDFFF] itself. *)
  val transitions_in : t -> state_id -> lo:int -> hi:int -> (Ucharset.t * state_id) list

  (** The automaton as data. [classes] are the coarsest partition of
      the codespace that every state's {!transitions} respect, in
      ascending order of least codepoint; [next] is
      [num_states * Array.length classes] entries, row major, holding
      the state a character of that class moves to, and [-1] where the
      state stops.

      Two characters in one class go to the same state from every
      state, which makes the table correct. Two characters that behave
      alike can still be in different classes: the partition is built
      from each state's transition charsets as stored, so when a state
      has two transitions to the same destination, their charsets stay
      in separate classes. That adds cells without affecting
      correctness — measured on random rule sets, merging transitions
      by destination first would remove about an eighth of the
      classes, and none on a lexer.

      A generator emits the classes once, maps each input character to
      its class, and indexes [next]; [Ucharset.to_packed_string]
      embeds a class as a string constant instead of a list of
      interval literals. There are usually far fewer classes than
      states (46 over 1739 for a lexer with 400 keywords), so this is
      one array, where emitting {!transitions} gives a function per
      state.

      When the states share little transition structure, the number
      of classes grows with the number of states and the table has
      [num_states * Array.length classes] cells, so check that size
      before emitting it. *)
  type table =
    { classes : Ucharset.t array
    ; next : int array
    }

  val table : t -> table
  val pp : Format.formatter -> t -> unit
end
