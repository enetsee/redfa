(* -- redfa --------------------------------------------------------------------

   A regex engine over the Unicode codespace, with the boolean
   operations (intersection, complement) alongside the usual ones, and
   DFA construction by Brzozowski derivative.

   {!Regex} is where a caller builds or parses a regex. {!Dfa} turns a
   list of them into an automaton. {!Ast} is the normal form the
   engine derives over, exposed for tests and benchmarks.
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

  val tag : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  (* -- constants and predicates -------------------------------------------- *)

  val empty : t
  val eps : t
  val any : t
  val is_empty : t -> bool
  val is_eps : t -> bool
  val is_chars : t -> bool
  val is_nullable : t -> bool
  val is_seq : t -> bool
  val is_alt : t -> bool

  (* -- sorted-distinct list helpers (sorted by tag) ------------------------ *)

  val sort_distinct : t list -> t list

  (* -- smart constructors -------------------------------------------------- *)

  val chars : Ucharset.t -> t
  val singleton : int -> t
  val range : lo:int -> hi:int -> t
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

  (* Children of a [Seq] or an [Alt], or the node in a singleton list. *)
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

  (* The codepoints of [s], decoded as UTF-8, in sequence. *)
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
       concat := repeat*
       repeat := prefix ('*' | '+' | '?')*
       prefix := '~' prefix | atom
       atom   := '(' alt ')' | '[' class ']' | '.' | escape | literal

     Escapes are [\t], [\n], [\r], [\f], [\0], [\u{HHHH}], the shorthand
     classes [\d], [\w], [\s] with their negations, and a backslash
     before ASCII punctuation for that character literally.
     ------------------------------------------------------------------------ *)

  type error =
    { pos : int (* byte offset into the source *)
    ; msg : string
    }

  val of_string : string -> (t, error) result

  (* The source with a caret under the offending byte. *)
  val error_to_string : string -> error -> string

  (* -- lowering and comparison --------------------------------------------- *)

  (* The source form {!of_string} reads back. *)
  val to_string : t -> string

  val to_ast : t -> Ast.t

  (* Whether two regexes denote the same language up to the normal
     form's associativity, commutativity and idempotence. Exact for
     terms differing only by those, conservative otherwise. *)
  val equivalent : t -> t -> bool

  (* -- emission ------------------------------------------------------------ *)

  (* Oniguruma source. [Error] where the term has no Oniguruma form: a
     [Complement], an [Inter] over anything but charsets, or the empty
     language. *)
  val to_oniguruma : t -> (string, string) result

  (* -- pretty-printing ----------------------------------------------------- *)

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

  (* Always 0, for symmetry with {!num_states}. *)
  val initial : t -> state_id

  val num_states : t -> int

  (* Case ids whose regex is nullable in this state, in ascending
     order: the tokens this state accepts. *)
  val accepts : t -> state_id -> int list

  (* Case ids still present in this state's item set: the tokens still
     reachable from here. A superset of {!accepts}. *)
  val reaches : t -> state_id -> int list

  (* Outgoing transitions. Each [(charset, dest)] means any codepoint in
     [charset] goes to [dest]. The charsets are pairwise disjoint, and
     arrive in ascending order of least codepoint. A codepoint in none
     of them has no transition. *)
  val transitions : t -> state_id -> (Ucharset.t * state_id) list

  (* Accepts nothing and goes nowhere. *)
  val is_dead : t -> state_id -> bool

  val iter_states : t -> (state_id -> unit) -> unit

  (* Collapses states that accept the same tokens and, on every input,
     go to equivalent states.

     Construction already quotients terms by associativity,
     commutativity and idempotence, so what this finds is two terms
     denoting the same language through different structure.

     The initial state of the result is the one holding the original
     initial state, and the rest are numbered by breadth-first search
     from it, so the numbering is canonical for a given input.
     {!reaches} at a merged state is the union of those merged.
     Idempotent. *)
  val minimise : t -> t

  val pp : Format.formatter -> t -> unit
end
