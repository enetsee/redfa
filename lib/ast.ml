(* -- normal form --------------------------------------------------------------

   The hash-consed regex the derivative engine works over. Smart
   constructors flatten nested [Seq]/[Alt]/[Inter] and sort [Alt] and
   [Inter] children by tag, dropping duplicates (associativity,
   commutativity, idempotence). Regexes differing only by those share
   a node, so equality is pointer equality, and deriving reaches a
   finite set of terms, which is what makes DFA construction
   terminate.

   Callers build regexes through the surface syntax and reach this
   through its lowering. Nothing here is stable.
   -------------------------------------------------------------------------- *)

type t =
  { tag : int
  ; node : node
  ; nullable : bool (* [is_nullable]; a bottom-up property, so computed at intern time *)
  ; mutable approx : Ucharset.Partition.t option
    (* memoised [approx_partition]; see the comment there *)
  ; mutable first : fmemo (* memoised [first_set]; see the comment there *)
  }

(* The first set and the bounds [deriv] guards with, in one immutable
   block so a single pointer write publishes all three. *)
and fmemo =
  { fs : Ucharset.t
  ; flo : int (* least element of [fs] *)
  ; fhi : int (* greatest element of [fs]; [flo > fhi] when empty *)
  }

and node =
  | Chars of Ucharset.t
  | Seq of t list
  | Alt of t list
  | Inter of t list
  | Not of t
  | Star of t

(* The sentinel every node starts with. Compared by address, so an
   empty first set is still distinguishable from an uncomputed one. *)
let no_fmemo = { fs = Ucharset.empty; flo = 1; fhi = 0 }

(* -- comparison and hashing ------------------------------------------------ *)

let node (t : t) = t.node
let tag (t : t) = t.tag
let equal (a : t) (b : t) = a == b
let compare (a : t) (b : t) = Stdlib.compare a.tag b.tag
let hash (t : t) = t.tag

(* -- hash-cons table ----------------------------------------------------------

   A weak hash table handing every distinct node a stable integer tag,
   so structurally equal regexes collapse to one shared record.
   Entries are held weakly, so a node goes away once nothing outside
   the table references it, taking its [approx] and [first] memos with
   it. A collected node is unreachable by definition, so nothing can
   observe that an equal node interned later gets a different tag.

   The technique is Conchon & Filliatre, "Type-Safe Modular
   Hash-Consing", ACM SIGPLAN Workshop on ML, 2006.

   Children of any node we look up are hash-consed already, smart
   constructors only building nodes from existing [t] values, so
   equality is shallow: two children are equal exactly when [==].
   -------------------------------------------------------------------------- *)

(* Multiplicative bit-mixer: xor in the next word, multiply by the
   32-bit golden ratio constant, fold the high bits back with an
   xorshift. *)
let[@inline] mix h x =
  let h = h lxor x * 0x9E3779B1 in
  h lxor (h lsr 29)
;;

(* splitmix64's finaliser. [mix] carries bits upwards, and this brings
   them back down. *)
let[@inline] final h =
  let h = h lxor (h lsr 30) * 0x3F58476D1CE4E5B9 in
  let h = h lxor (h lsr 27) * 0x14D049BB133111EB in
  h lxor (h lsr 31)
;;

let node_hash (n : node) =
  match n with
  | Chars c -> final (mix 0x5eed_5eed (Ucharset.hash c))
  | Seq xs -> List.fold_left (fun h x -> mix h x.tag) 0x5eed_5eed xs |> final
  | Alt xs -> List.fold_left (fun h x -> mix h x.tag) 0xa17e_a17e xs |> final
  | Inter xs -> List.fold_left (fun h x -> mix h x.tag) 0x1e7e_1e7e xs |> final
  | Not x -> final (mix 0xb007_b007 x.tag)
  | Star x -> final (mix 0xc0de_c0de x.tag)
;;

let rec list_phys_equal xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xt, y :: yt -> x == y && list_phys_equal xt yt
  | _ -> false
;;

let node_equal (a : node) (b : node) =
  match a, b with
  | Chars c1, Chars c2 -> Ucharset.equal c1 c2
  | Seq xs, Seq ys -> list_phys_equal xs ys
  | Alt xs, Alt ys -> list_phys_equal xs ys
  | Inter xs, Inter ys -> list_phys_equal xs ys
  | Not x, Not y -> x == y
  | Star x, Star y -> x == y
  | _ -> false
;;

type table =
  { mutable buckets : t Weak.t array
  ; mutable size : int
    (* live entries at the last resize, plus insertions since; exact
       live count right after one *)
  }

let min_buckets = 16

(* Resize once the average bucket would hold more than [load] entries. *)
let load = 2

(* Every bucket slot starts out pointing at one shared empty weak
   array. [bucket_put] replaces an empty bucket wholesale. *)
let make_table capacity =
  let rec pow2 k = if k >= capacity then k else pow2 (k * 2) in
  let n = pow2 min_buckets in
  { buckets = Array.make n (Weak.create 0); size = 0 }
;;

(* Bucket counts stay powers of two, so the index is a mask. *)
let bucket_index hkey n = hkey land max_int land (n - 1)
let grow_cap cap = min (if cap = 0 then 2 else cap * 2) (Sys.max_array_length - 1)

(* Add [v] to bucket [i], reusing a slot the GC has vacated if there is
   one and growing the bucket if there isn't. *)
let bucket_put (buckets : t Weak.t array) i (v : t) =
  let b = buckets.(i) in
  let cap = Weak.length b in
  let free = ref (-1) in
  let j = ref 0 in
  while !free < 0 && !j < cap do
    if Weak.check b !j then incr j else free := !j
  done;
  if !free >= 0
  then Weak.set b !free (Some v)
  else (
    let cap' = grow_cap cap in
    let b' = Weak.create cap' in
    Weak.blit b 0 b' 0 cap;
    Weak.set b' cap (Some v);
    buckets.(i) <- b')
;;

let live_count (buckets : t Weak.t array) =
  let live = ref 0 in
  Array.iter
    (fun b ->
       for j = 0 to Weak.length b - 1 do
         if Weak.check b j then incr live
       done)
    buckets;
  !live
;;

(* One bucket per live entry, rounded up to a power of two. That is the
   occupancy the doubling policy settled at anyway; the difference is
   that it can now be reached from above. *)
let buckets_for live =
  let cap = Sys.max_array_length - 1 in
  let rec pow2 k = if k >= live || k * 2 > cap then k else pow2 (k * 2) in
  pow2 min_buckets
;;

let rebuild tbl n' =
  let old = tbl.buckets in
  let fresh = Array.make n' (Weak.create 0) in
  Array.iter
    (fun b ->
       for j = 0 to Weak.length b - 1 do
         match Weak.get b j with
         | Some v -> bucket_put fresh (bucket_index (node_hash v.node) n') v
         | None -> ()
       done)
    old;
  tbl.buckets <- fresh
;;

(* Resize from the entries that are still live rather than from the
   number interned since the last resize. Entries die on their own,
   being weak, so the table shrinks here as readily as it grows: this
   is where it learns how much of itself is still in use. Rehashing
   costs [O(buckets + live)] and leaves room for [live] more insertions
   before the next one, so it stays amortised constant. *)
let rehash tbl =
  let live = live_count tbl.buckets in
  let n' = buckets_for live in
  if n' <> Array.length tbl.buckets then rebuild tbl n';
  (* [size] is now live-at-last-resize plus insertions since, so the
     next resize is triggered by growth in live entries rather than by
     cumulative interning. *)
  tbl.size <- live
;;

(* Also the size [clear_cache] returns to. *)
let initial_buckets = 1024
let table = make_table initial_buckets
let next_tag = ref 0

(* Every child of [n] is interned by the time we get here, so
   nullability costs one field read per child. *)
let nullable_of (n : node) =
  match n with
  | Chars _ -> false
  | Seq xs -> List.for_all (fun t -> t.nullable) xs
  | Alt xs -> List.exists (fun t -> t.nullable) xs
  | Inter xs -> List.for_all (fun t -> t.nullable) xs
  | Not x -> not x.nullable
  | Star _ -> true
;;

(* Compare [n] against the stored nodes directly. Only a miss builds a
   record. *)
let intern (n : node) : t =
  let nb = Array.length table.buckets in
  let i = bucket_index (node_hash n) nb in
  let b = table.buckets.(i) in
  let cap = Weak.length b in
  let found = ref None in
  let j = ref 0 in
  while Option.is_none !found && !j < cap do
    (match Weak.get b !j with
     | Some e when node_equal e.node n -> found := Some e
     | _ -> ());
    incr j
  done;
  match !found with
  | Some e -> e
  | None ->
    let v =
      { tag = !next_tag
      ; node = n
      ; nullable = nullable_of n
      ; approx = None
      ; first = no_fmemo
      }
    in
    incr next_tag;
    bucket_put table.buckets i v;
    table.size <- table.size + 1;
    if table.size > load * nb then rehash table;
    v
;;

(* -- constants and basic predicates ---------------------------------------- *)

let empty = intern (Chars Ucharset.empty)
let eps = intern (Seq [])
let any = intern (Chars Ucharset.all)

(* Drop every entry and return the bucket array to its initial size.

   [rehash] re-measures the table only when something is interned, so a
   program that builds a large automaton, drops it and then stops
   interning holds the array at its peak for the rest of its life.
   Since the table is global there is nothing for the caller to release
   instead, so this is the release.

   The three constants above are put back, being reachable for the life
   of the program either way: without that, interning [Chars empty]
   again would mint a second record and the [empty] [deriv] returns
   would stop being the [empty] a caller holds.

   Nothing else is. A node interned before the call and one interned
   after are separate records even when structurally equal, so [equal]
   answers false for the pair and the [Alt]/[Inter] canonical form,
   which is maintained by tag, no longer holds across the boundary.
   Tags themselves stay unique — the counter is untouched — so nothing
   is mistaken for anything else. Call this only once every regex and
   DFA built so far has been dropped.

   {!Siesta.Cache.clear} is the same operation on a cache the caller
   owns, which is the sounder shape; here the table is global. *)
let clear_cache () =
  table.buckets <- Array.make initial_buckets (Weak.create 0);
  table.size <- 0;
  List.iter
    (fun v ->
       bucket_put table.buckets (bucket_index (node_hash v.node) initial_buckets) v;
       table.size <- table.size + 1)
    [ empty; eps; any ]
;;

let is_empty (t : t) =
  match t.node with
  | Chars c -> Ucharset.is_empty c
  | _ -> false
;;

let is_chars (t : t) =
  match t.node with
  | Chars _ -> true
  | _ -> false
;;

let is_eps (t : t) =
  match t.node with
  | Seq [] -> true
  | _ -> false
;;

let is_nullable (t : t) = t.nullable

(* -- sorted-distinct list helpers (sorted by t.tag) ------------------------ *)

let cmp_tag (a : t) (b : t) = Stdlib.compare a.tag b.tag

(* Insert [t] into a list already sorted ascending by tag, dropping
   any element with the same tag (set-semantics). *)
let rec sorted_insert (t : t) = function
  | [] -> [ t ]
  | x :: rest as l ->
    let c = cmp_tag t x in
    if c = 0 then l else if c < 0 then t :: l else x :: sorted_insert t rest
;;

(* Merge two sorted-ascending lists, dropping duplicates by tag. *)
let rec sorted_merge xs ys =
  match xs, ys with
  | [], l | l, [] -> l
  | x :: xt, y :: yt ->
    let c = cmp_tag x y in
    if c = 0
    then x :: sorted_merge xt yt
    else if c < 0
    then x :: sorted_merge xt ys
    else y :: sorted_merge xs yt
;;

(* Lists of two or fewer are handled without sorting. *)
let sort_distinct ts =
  match ts with
  | [] | [ _ ] -> ts
  | [ a; b ] ->
    let c = cmp_tag a b in
    if c = 0 then [ a ] else if c < 0 then ts else [ b; a ]
  | _ ->
    let sorted = List.sort cmp_tag ts in
    let rec dedup = function
      | [] -> []
      | [ x ] -> [ x ]
      | x :: (y :: _ as rest) when x.tag = y.tag -> dedup rest
      | x :: rest -> x :: dedup rest
    in
    dedup sorted
;;

(* -- smart constructors -------------------------------------------------------

   Each constructor preserves these invariants on the in-memory
   representation:

   - [Seq] children are never themselves [Seq] (flattening) and
     never [eps] (identity dropping); a [Seq] of length 0 is the
     canonical [eps], length 1 is unwrapped to its sole child, an
     [empty] anywhere makes the whole sequence [empty].
   - [Alt] children are sorted ascending by tag, distinct, length
     ≥ 2 (singletons unwrap), no [empty] (identity dropping). At
     most one child is a [Chars] (others are merged into it).
   - [Inter] children: sorted distinct, length ≥ 2, [empty]
     anywhere makes the whole [empty]. A [Chars all] child is
     *kept*. It restricts the intersection to single codepoint
     strings (see the note above [inters]).
   - [Not (Not x)] is normalised to [x].
   - [Star (Star x)] is [Star x]; [Star empty] and [Star eps] are
     [eps].
   -------------------------------------------------------------------------- *)

let chars c = if Ucharset.is_empty c then empty else intern (Chars c)
let singleton cp = chars (Ucharset.singleton cp)
let range ~lo ~hi = chars (Ucharset.range ~lo ~hi)

(* The children of a [Seq], or the term in a singleton list. *)
let seq_children t =
  match t.node with
  | Seq xs -> xs
  | _ -> [ t ]
;;

let seqs ts =
  if List.exists is_empty ts
  then empty
  else (
    let flat = List.concat_map seq_children ts in
    let nonemp = List.filter (fun t -> not (is_eps t)) flat in
    match nonemp with
    | [] -> eps
    | [ x ] -> x
    | xs -> intern (Seq xs))
;;

let is_seq t =
  match t.node with
  | Seq _ -> true
  | _ -> false
;;

(* Handle two terms directly. [is_eps] is tested before [is_seq],
   since [eps] is the canonical [Seq []]. *)
let seq a b =
  if is_empty a || is_empty b
  then empty
  else if is_eps a
  then b
  else if is_eps b
  then a
  else if is_seq a || is_seq b
  then seqs [ a; b ]
  else intern (Seq [ a; b ])
;;

(* [seqs] specialised to a suffix of a canonical [Seq]'s children,
   which are already flat and hold no [eps] or [empty]. *)
let seq_suffix = function
  | [] -> eps
  | [ x ] -> x
  | xs -> intern (Seq xs)
;;

(* One Ucharset singleton per UTF-8 codepoint, so [str "λ"] denotes
   U+03BB. *)
let str s =
  let cps = ref []
  and i = ref 0
  and n = String.length s in
  while !i < n do
    let d = String.get_utf_8_uchar s !i in
    (* U+FFFD is what a bad byte decodes to, so accepting it silently
       builds a different regex. Raise instead, as the parser does. *)
    if not (Uchar.utf_decode_is_valid d) then invalid_arg "Redfa.Ast.str: malformed UTF-8";
    cps := singleton (Uchar.to_int (Uchar.utf_decode_uchar d)) :: !cps;
    i := !i + Uchar.utf_decode_length d
  done;
  seqs (List.rev !cps)
;;

let star t =
  match t.node with
  | Star _ -> t
  | _ when is_empty t || is_eps t -> eps
  | _ -> intern (Star t)
;;

(* For Alt: collect any Chars children, merge them into a single
   Chars; combine the rest into a sorted-distinct list. *)
let alt_children t =
  match t.node with
  | Alt xs -> xs
  | _ -> [ t ]
;;

(* Collect and union in one pass. *)
let split_chars ts =
  let cs = ref []
  and rest = ref [] in
  List.iter
    (fun t ->
       match t.node with
       | Chars c -> if not (Ucharset.is_empty c) then cs := c :: !cs
       | _ -> rest := t :: !rest)
    ts;
  (* Zero or one set needs no builder. *)
  ( (match !cs with
     | [] -> Ucharset.empty
     | [ c ] -> c
     | cs -> Ucharset.union_list cs)
  , !rest )
;;

let is_alt t =
  match t.node with
  | Alt _ -> true
  | _ -> false
;;

let alts ts =
  (* Canonical [Alt] children are never themselves [Alt], so the copy
     is skipped unless one is. *)
  let flat = if List.exists is_alt ts then List.concat_map alt_children ts else ts in
  let cs, rest = split_chars flat in
  (* [rest] holds no [Chars] nodes, [split_chars] having routed them
     all into [cs], so nothing in it can be [empty]. *)
  let rest = sort_distinct rest in
  (* Re-introduce the merged Chars as a single child if non-empty. *)
  let children = if Ucharset.is_empty cs then rest else sorted_insert (chars cs) rest in
  match children with
  | [] -> empty
  | [ x ] -> x
  | xs -> intern (Alt xs)
;;

let alt a b = alts [ a; b ]

(* For Inter, the same treatment, intersecting the Chars children and
   propagating empty. *)
let inter_children t =
  match t.node with
  | Inter xs -> xs
  | _ -> [ t ]
;;

(* Inside an [Inter], [Chars all] denotes the single character
   strings, so [inter any X] restricts X to its single character
   members. The flag records whether any [Chars] child contributed,
   which separates "no Ucharset constraint supplied" from "the
   supplied Ucharsets intersected to [all]". *)
let split_chars_for_inter ts =
  let cs = ref []
  and rest = ref [] in
  List.iter
    (fun t ->
       match t.node with
       | Chars c -> cs := c :: !cs
       | _ -> rest := t :: !rest)
    ts;
  (* [inter_list] gives [all] on the empty list. *)
  ( !cs <> []
  , (match !cs with
     | [] -> Ucharset.all
     | [ c ] -> c
     | cs -> Ucharset.inter_list cs)
  , !rest )
;;

let inters ts =
  match ts with
  | [] ->
    (* Identity for intersection is the universal language Σ*. *)
    star any
  | _ ->
    if List.exists is_empty ts
    then empty
    else (
      let flat = List.concat_map inter_children ts in
      let saw_chars, cs, rest = split_chars_for_inter flat in
      if saw_chars && Ucharset.is_empty cs
      then empty
      else (
        let rest = sort_distinct rest in
        let children = if saw_chars then sorted_insert (chars cs) rest else rest in
        match children with
        | [] ->
          (* Unreachable: [flat] is non-empty, and either branch
             above leaves a child behind. *)
          star any
        | [ x ] -> x
        | xs -> intern (Inter xs)))
;;

let inter a b = inters [ a; b ]

let complement t =
  match t.node with
  | Not x -> x
  | _ -> intern (Not t)
;;

let plus t = seq t (star t)
let opt t = alt t eps

(* -- derivative ---------------------------------------------------------------

   Brzozowski derivative: [deriv r ~uchr] is a regex whose language is
   { s | (uchr :: s) in L(r) }.
   -------------------------------------------------------------------------- *)

(* -- first sets ---------------------------------------------------------------

   [first_set r] over-approximates the codepoints that can begin a
   word of [L(r)]. Anything outside it derives to [empty], making it a
   guard on [deriv]. Under [Not] the guard is [all], [deriv (Not x)]
   being [Not (deriv x)], a live node whatever its language.

   Computed on first use and cached on the node.
   -------------------------------------------------------------------------- *)

let rec fmemo_of (r : t) =
  if r.first != no_fmemo
  then r.first
  else (
    let c =
      match r.node with
      | Chars c -> c
      | Seq xs ->
        (* The nullable prefix, plus the first element after it.
           Nothing past that can start a word. *)
        let rec go acc = function
          | [] -> acc
          | x :: rest ->
            let acc = first_set x :: acc in
            if x.nullable then go acc rest else acc
        in
        Ucharset.union_list (go [] xs)
      | Alt xs -> Ucharset.union_list (List.map first_set xs)
      | Inter xs ->
        (* [deriv] intersects the children's derivatives, so a codepoint
           missing from any one of them makes the whole thing [empty]. *)
        Ucharset.inter_list (List.map first_set xs)
      | Not _ -> Ucharset.all
      | Star x -> first_set x
    in
    (* Bounds for the guard in [deriv]. An empty first set keeps the
       sentinel's [1 > 0], rejecting every codepoint. *)
    let m =
      match Ucharset.min_elt_opt c, Ucharset.max_elt_opt c with
      | Some lo, Some hi -> { fs = c; flo = lo; fhi = hi }
      | _ -> { fs = c; flo = 1; fhi = 0 }
    in
    r.first <- m;
    m)

and first_set (r : t) = (fmemo_of r).fs

(* Two integer comparisons against the first set's bounds reject a
   codepoint outside it. [flo = fhi] is a single codepoint first
   set, where the bounds have already settled membership. *)
and deriv (r : t) ~uchr =
  let m = fmemo_of r in
  if uchr < m.flo || uchr > m.fhi
  then empty
  else if m.flo = m.fhi || Ucharset.mem m.fs uchr
  then deriv_node r ~uchr
  else empty

and deriv_node (r : t) ~uchr =
  match r.node with
  | Chars c -> if Ucharset.mem c uchr then eps else empty
  | Seq xs -> deriv_seq xs ~uchr
  (* Collect the non-[empty] derivatives. Accumulating reverses the
     order, and [alts] sorts its children by tag. *)
  | Alt xs ->
    let rec collect acc = function
      | [] -> alts acc
      | x :: rest ->
        let d = deriv x ~uchr in
        collect (if is_empty d then acc else d :: acc) rest
    in
    collect [] xs
  (* Same, and an [empty] derivative annihilates an intersection, so
     it stops there. *)
  | Inter xs ->
    let rec collect acc = function
      | [] -> inters acc
      | x :: rest ->
        let d = deriv x ~uchr in
        if is_empty d then empty else collect (d :: acc) rest
    in
    collect [] xs
  | Not x -> complement (deriv x ~uchr)
  | Star r' -> seq (deriv r' ~uchr) r

and deriv_seq xs ~uchr =
  match xs with
  | [] -> empty
  | [ x ] -> deriv x ~uchr
  | x :: rest ->
    let head = seq (deriv x ~uchr) (seq_suffix rest) in
    if x.nullable then alt head (deriv_seq rest ~uchr) else head
;;

(* -- approximate charset partition --------------------------------------------

   A partition of the codespace where any two codepoints in one block
   derive [r] to structurally identical regexes (relex's "approximate
   character sets", two codepoints in different blocks being free to
   agree as well). Combining two is their common refinement, which
   [Ucharset.Partition.meet_all] takes in a balanced tree.

   Staying in [Ucharset.Partition.t] lets [approx_representatives]
   give a DFA loop one codepoint per block without building any set.
   [approx_charset] builds the sets for callers wanting transition
   labels, block [i] matching representative [i]. Both memoise on the
   node.
   -------------------------------------------------------------------------- *)

let rec approx_partition (r : t) : Ucharset.Partition.t =
  match r.approx with
  | Some p -> p
  | None ->
    let p =
      match r.node with
      (* {c, complement c}, which folds away the [empty] and [all]
         cases too. Blocks come numbered by least element, so
         [of_set] gives exactly this. *)
      | Chars c -> Ucharset.Partition.of_set c
      | Seq xs -> approx_seq xs
      | Alt xs | Inter xs -> Ucharset.Partition.meet_all (List.map approx_partition xs)
      | Not x | Star x -> approx_partition x
    in
    r.approx <- Some p;
    p

(* Only the nullable prefix of a [Seq] contributes. A derivative
   reaches as far as the first non-nullable element, making that the
   last one whose partition matters. *)
and approx_seq xs =
  let rec prefix acc = function
    | [] -> List.rev acc
    | x :: rest ->
      let acc = approx_partition x :: acc in
      if x.nullable then prefix acc rest else List.rev acc
  in
  Ucharset.Partition.meet_all (prefix [] xs)
;;

(* One codepoint per block, in increasing order. Builds no sets. *)
let approx_representatives (r : t) : int list =
  Ucharset.Partition.representatives (approx_partition r)
;;

(* The blocks themselves, in the same order as the representatives. *)
let approx_charset (r : t) : Ucharset.t list =
  Ucharset.Partition.blocks (approx_partition r)
;;

(* -- deciding a language ------------------------------------------------------

   Emptiness and equivalence of the language, not of the term:
   [equal] separates [a*a*] from [a*], and [is_empty] misses [a & ~a].

   A node and [deriv] are a deterministic automaton, so both traverse
   one instead of building it. States are nodes, acceptance is
   [nullable], the alphabet is [approx_partition]. Derivatives are
   finite, so both terminate. Neither is bounded, and each costs about
   what building the DFA costs.
   -------------------------------------------------------------------------- *)

(* Reachable derivatives, until one is nullable: it accepts the empty
   string, so the input that reached it matches. [empty] derives to
   itself and accepts nothing, so it is pruned. *)
let is_empty_language (r : t) =
  if is_empty r
  then true
  else (
    let seen : (int, unit) Hashtbl.t = Hashtbl.create 64 in
    Hashtbl.add seen r.tag ();
    let rec go = function
      | [] -> true
      | x :: rest ->
        if x.nullable
        then false
        else (
          let p = approx_partition x in
          let todo = ref rest in
          for i = Ucharset.Partition.num_blocks p - 1 downto 0 do
            let d = deriv x ~uchr:(Ucharset.Partition.representative p i) in
            if (not (is_empty d)) && not (Hashtbl.mem seen d.tag)
            then (
              Hashtbl.add seen d.tag ();
              todo := d :: !todo)
          done;
          go !todo)
    in
    go [ r ])
;;

(* Union-find over node tags, for [equivalent]. A tag absent from
   [parent] is its own class, so only merges are stored. Union by
   size, path compression in [find]. *)
type uf =
  { parent : (int, int) Hashtbl.t
  ; size : (int, int) Hashtbl.t (* class size, at the root only *)
  }

let uf_create n = { parent = Hashtbl.create n; size = Hashtbl.create n }

let rec uf_find uf x =
  match Hashtbl.find_opt uf.parent x with
  | None -> x
  | Some p ->
    let r = uf_find uf p in
    if r <> p then Hashtbl.replace uf.parent x r;
    r
;;

let uf_size uf x =
  match Hashtbl.find_opt uf.size x with
  | Some s -> s
  | None -> 1
;;

(* [a] and [b] are roots of distinct classes. *)
let uf_union uf a b =
  let sa = uf_size uf a
  and sb = uf_size uf b in
  let big, small = if sa >= sb then a, b else b, a in
  Hashtbl.replace uf.parent small big;
  Hashtbl.replace uf.size big (sa + sb)
;;

(* Assume the two match the same strings, then look for one that tells
   them apart. Pairs come off a stack. Sides that disagree on
   [nullable] mean no. A pair already in one class was assumed equal,
   so skip it. Otherwise merge, and push the derivatives, one per
   block of the two partitions met together.

   Skipping merged pairs is what beats the reachable product: one
   expansion per node at most, against one per pair of nodes. On
   automata both far from minimal that is [p + q] against [p * q].
   [(a{63})*a*] against [(a{64})*a*], both [a*], is 127 pairs and 0.13
   ms here against 4958 and 3.5 ms; level on the four token sets in
   [bench], where the automata run in lockstep.

   Hopcroft & Karp, "A Linear Algorithm for Testing Equivalence of
   Finite Automata", 1971; the relation it accumulates is a
   bisimulation up to equivalence (Bonchi & Pous, POPL 2013). *)
let equivalent (a : t) (b : t) =
  if equal a b
  then true
  else (
    let uf = uf_create 64 in
    let rec go = function
      | [] -> true
      | (x, y) :: rest ->
        let rx = uf_find uf x.tag
        and ry = uf_find uf y.tag in
        if rx = ry
        then go rest
        else if x.nullable <> y.nullable
        then false
        else (
          uf_union uf rx ry;
          let p = Ucharset.Partition.meet (approx_partition x) (approx_partition y) in
          let todo = ref rest in
          for i = Ucharset.Partition.num_blocks p - 1 downto 0 do
            let c = Ucharset.Partition.representative p i in
            let dx = deriv x ~uchr:c
            and dy = deriv y ~uchr:c in
            (* One node is one class already, so the pair tells us
               nothing. Skipping it saves two [find]s. *)
            if not (equal dx dy) then todo := (dx, dy) :: !todo
          done;
          go !todo)
    in
    go [ a, b ])
;;

(* -- pretty-printing ------------------------------------------------------- *)

let prec_of (t : t) =
  match t.node with
  | Star _ -> 5
  | Not _ -> 4
  | Seq _ -> 3
  | Inter _ -> 2
  | Alt _ -> 1
  | Chars _ -> 6
;;

let rec pp_prec prec ppf t =
  let p = prec_of t in
  if p < prec
  then Format.fprintf ppf "(%a)" (pp_prec 0) t
  else (
    match t.node with
    | Chars c -> Ucharset.pp ppf c
    | Seq [] -> Format.fprintf ppf "ε"
    | Seq xs ->
      Format.fprintf
        ppf
        "@[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,") (pp_prec 3))
        xs
    | Alt xs ->
      Format.fprintf
        ppf
        "@[<hov>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ |@ ")
           (pp_prec 1))
        xs
    | Inter xs ->
      Format.fprintf
        ppf
        "@[<hov>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ &@ ")
           (pp_prec 2))
        xs
    | Not x -> Format.fprintf ppf "~%a" (pp_prec 4) x
    | Star x -> Format.fprintf ppf "%a*" (pp_prec 5) x)
;;

let pp ppf t = pp_prec 0 ppf t

(* -- matching -------------------------------------------------------------- *)

(* Fold [deriv] over the codepoints of [s], stopping early once the
   residual is [empty] and nothing can bring it back. *)
let eval (r : t) (s : string) =
  let n = String.length s in
  let rec go r i =
    if is_empty r
    then false
    else if i >= n
    then r.nullable
    else (
      let d = String.get_utf_8_uchar s i in
      let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
      go (deriv r ~uchr:cp) (i + Uchar.utf_decode_length d))
  in
  go r 0
;;
