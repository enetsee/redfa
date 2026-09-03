(* Internal interface. The surface is re-exported by {!Redfa.Ast} and
   documented there; see redfa.mli. *)

type t

val tag : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val clear_cache : unit -> unit
val empty : t
val eps : t
val any : t
val is_empty : t -> bool
val is_eps : t -> bool
val is_chars : t -> bool
val is_nullable : t -> bool
val is_seq : t -> bool
val is_alt : t -> bool
val sort_distinct : t list -> t list
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
val seq_children : t -> t list
val alt_children : t -> t list
val first_set : t -> Ucharset.t
val deriv : t -> uchr:int -> t
val eval : t -> string -> bool
val approx_partition : t -> Ucharset.Partition.t
val approx_representatives : t -> int list
val approx_charset : t -> Ucharset.t list
val is_empty_language : t -> bool
val equivalent : t -> t -> bool
val pp : Format.formatter -> t -> unit
