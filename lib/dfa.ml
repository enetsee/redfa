(* DFA construction by item-set derivative with state memoisation,
   following relex's [Dfa.of_rule] shape over stdlib and arrays.

   A state is a sorted-distinct list of [{case_id; regex}] items,
   compared on [regex.tag], which hash-consing makes canonical. A
   worklist drives discovery, and its tables become flat arrays once
   it drains. *)

type state_id = int

type item =
  { case_id : int
  ; regex : Ast.t
  }

(* Order by case_id, then by regex tag. Runs under [List.sort_uniq]
   once per transition built. *)
let item_compare (a : item) (b : item) =
  let c = Int.compare a.case_id b.case_id in
  if c <> 0 then c else Int.compare (Ast.tag a.regex) (Ast.tag b.regex)
;;

let canonicalise_items items = List.sort_uniq item_compare items

(* Drop adjacent duplicates from an ascending int list. Items are
   canonicalised by (case_id, regex tag), so the case_ids reaching
   here already ascend. *)
let rec dedup_sorted = function
  | [] -> []
  | [ _ ] as l -> l
  | x :: (y :: _ as rest) ->
    if (x : int) = y then dedup_sorted rest else x :: dedup_sorted rest
;;

(* Key a state by its item list. Two items match when both their
   case_id and their regex tag match, and the hash mixes both. *)
module State_key = struct
  type t = item list

  let rec equal_list xs ys =
    match xs, ys with
    | [], [] -> true
    | x :: xt, y :: yt ->
      x.case_id = y.case_id && Ast.tag x.regex = Ast.tag y.regex && equal_list xt yt
    | _ -> false
  ;;

  let equal = equal_list
  let mix h x = h * 31 lxor x land max_int

  let hash xs =
    List.fold_left (fun h it -> mix (mix h it.case_id) (Ast.tag it.regex)) 0xd1fa_d1fa xs
  ;;
end

module State_table = Hashtbl.Make (State_key)

(* Dense storage indexed by state id. Ids are handed out
   sequentially from zero, so these are array indices. Grows by
   doubling. *)
type 'a vec =
  { mutable data : 'a array
  ; default : 'a
  }

let vec_make ~default n = { data = Array.make n default; default }

let vec_set v i x =
  let cap = Array.length v.data in
  if i >= cap
  then (
    let bigger = Array.make (max (i + 1) (2 * cap)) v.default in
    Array.blit v.data 0 bigger 0 cap;
    v.data <- bigger);
  v.data.(i) <- x
;;

let vec_get v i = v.data.(i)

type t =
  { num_states : int
  ; accepts : int list array
  ; reaches : int list array
  ; transitions : (Ucharset.t * state_id) list array
  }

let initial _ = 0
let num_states t = t.num_states
let accepts t id = t.accepts.(id)
let reaches t id = t.reaches.(id)
let transitions t id = t.transitions.(id)

let is_dead t id =
  match t.accepts.(id), t.transitions.(id) with
  | [], [] -> true
  | _ -> false
;;

let iter_states t f =
  for id = 0 to t.num_states - 1 do
    f id
  done
;;

(* -- construction ---------------------------------------------------------- *)

(* Joint approx partition for a state's items, the common refinement
   of each item's own. Stays in [Ucharset.Partition.t], so a chain of
   meets skips the intermediate blocks. The loop below builds every
   block of the result, in one [Partition.blocks] call, the
   transitions needing them as labels. An [empty] item contributes the
   single block covering the codespace, neutral under meet. *)
let approx_partition items =
  Ucharset.Partition.meet_all (List.map (fun it -> Ast.approx_partition it.regex) items)
;;

(* Derive every item's regex on [uchr], dropping items that collapse
   to [empty] (no path to acceptance through them). The accumulator
   comes out reversed, which is fine, [canonicalise_items] sorts. *)
let step_items items ~uchr =
  let rec go acc = function
    | [] -> acc
    | it :: rest ->
      let r' = Ast.deriv it.regex ~uchr in
      go (if Ast.is_empty r' then acc else { it with regex = r' } :: acc) rest
  in
  canonicalise_items (go [] items)
;;

(* Raised past the state budget and caught in {!of_tokens_within},
   which discards the tables built so far, leaving {!of_tokens}
   total. *)
exception Over_budget

(* [max_states] caps the states of the result, one integer compare per
   state discovered. The unbounded form passes [max_int], so the guard
   compares against it and leaves it alone ([max_int + 1] is
   negative). *)
let build (token_regexes : (int * Regex.t) list) ~max_states : t =
  let initial_items =
    canonicalise_items
      (List.filter_map
         (fun (cid, rx) ->
            let rx = Regex.to_ast rx in
            if Ast.is_empty rx then None else Some { case_id = cid; regex = rx })
         token_regexes)
  in
  let state_id_of_key : state_id State_table.t = State_table.create 64 in
  let key_of_id : item list vec = vec_make ~default:[] 64 in
  let next_id = ref 0 in
  let intern_state key =
    match State_table.find_opt state_id_of_key key with
    | Some id -> id, false
    | None ->
      let id = !next_id in
      incr next_id;
      if !next_id > max_states then raise Over_budget;
      State_table.add state_id_of_key key id;
      vec_set key_of_id id key;
      id, true
  in
  let initial_id, _ = intern_state initial_items in
  assert (initial_id = 0);
  let accepts_tbl : int list vec = vec_make ~default:[] 64 in
  let reaches_tbl : int list vec = vec_make ~default:[] 64 in
  let transitions_tbl : (Ucharset.t * state_id) list vec = vec_make ~default:[] 64 in
  let queue = Queue.create () in
  Queue.add initial_id queue;
  while not (Queue.is_empty queue) do
    let id = Queue.pop queue in
    let items = vec_get key_of_id id in
    (* Dedupe the case_id projections. A caller may pass two items
       with the same case_id and different regexes (say
       [(K_NUMBER, decimal); (K_NUMBER, hex)]). Those evolve
       separately under derivation, while accepts and reaches report
       each token at most once. *)
    let acc =
      dedup_sorted
        (List.filter_map
           (fun it -> if Ast.is_nullable it.regex then Some it.case_id else None)
           items)
    in
    let rch = dedup_sorted (List.map (fun it -> it.case_id) items) in
    vec_set accepts_tbl id acc;
    vec_set reaches_tbl id rch;
    let part = approx_partition items in
    let nb = Ucharset.Partition.num_blocks part in
    (* [Partition.blocks] builds every block in two passes over the
       segments. One call before the loop, indexed by block number. *)
    let block_sets =
      if nb = 0 then [||] else Array.of_list (Ucharset.Partition.blocks part)
    in
    (* Counting down, so consing leaves the transitions in ascending
       block order (which is ascending order of least codepoint, being
       how a partition numbers its blocks). *)
    let trans = ref [] in
    for i = nb - 1 downto 0 do
      match step_items items ~uchr:(Ucharset.Partition.representative part i) with
      | [] -> ()
      | next_items ->
        let dest_id, is_new = intern_state next_items in
        if is_new then Queue.add dest_id queue;
        trans := (block_sets.(i), dest_id) :: !trans
    done;
    vec_set transitions_tbl id !trans
  done;
  let n = !next_id in
  let accepts = Array.sub accepts_tbl.data 0 n in
  let reaches = Array.sub reaches_tbl.data 0 n in
  let transitions = Array.sub transitions_tbl.data 0 n in
  { num_states = n; accepts; reaches; transitions }
;;

let of_tokens (token_regexes : (int * Regex.t) list) : t =
  build token_regexes ~max_states:max_int
;;

let of_tokens_within ~max_states token_regexes =
  match build token_regexes ~max_states with
  | dfa -> Some dfa
  | exception Over_budget -> None
;;

(* -- minimisation -------------------------------------------------------------

   Moore's partition refinement, over an automaton trimmed of its dead
   states first. States start grouped by [accepts]. Each pass keeps
   two together when their previous block and their outgoing
   transition signature both match, and passes only split, so the
   block count climbs until it settles or every state sits alone.

   Construction collapses residuals that agree up to associativity,
   commutativity and idempotence, so the states arriving here are
   already distinct as terms. Two terms can still denote the same
   language through different structure, and those are what this pass
   merges.
   -------------------------------------------------------------------------- *)

(* The states some accepting state is reachable from. Everything else
   has an empty residual language: it accepts nothing, and no path out
   of it reaches anything that does.

   Backward breadth-first from the accepting states. Only the edges
   leaving a non-accepting state are reversed: a state that accepts
   something is live from the outset, so nothing is ever discovered
   through it, and on a lexer's automaton most states accept. The
   reverse edges go into three int arrays -- a counting sort over the
   destinations -- and the frontier into a fourth, so the scan
   allocates four flat arrays and nothing per edge. It runs on every
   [minimise], including the ones with nothing dead to find, which is
   what makes both economies worth having. *)
let live_states (dfa : t) =
  let n = dfa.num_states in
  let live = Array.make n false in
  let frontier = Array.make n 0 in
  let top = ref 0 in
  let discoverable = ref 0 in
  for id = 0 to n - 1 do
    match dfa.accepts.(id) with
    | [] -> incr discoverable
    | _ ->
      live.(id) <- true;
      frontier.(!top) <- id;
      incr top
  done;
  if !discoverable = 0
  then live
  else (
    (* [start.(i) .. start.(i + 1) - 1] indexes [preds] for state [i],
       listing the non-accepting states with an edge into it. *)
    let start = Array.make (n + 1) 0 in
    let edges = ref 0 in
    for id = 0 to n - 1 do
      if not live.(id)
      then
        List.iter
          (fun (_, dst) ->
             incr edges;
             start.(dst + 1) <- start.(dst + 1) + 1)
          dfa.transitions.(id)
    done;
    for i = 0 to n - 1 do
      start.(i + 1) <- start.(i + 1) + start.(i)
    done;
    let fill = Array.copy start in
    let preds = Array.make (max 1 !edges) 0 in
    for id = 0 to n - 1 do
      if not live.(id)
      then
        List.iter
          (fun (_, dst) ->
             preds.(fill.(dst)) <- id;
             fill.(dst) <- fill.(dst) + 1)
          dfa.transitions.(id)
    done;
    while !top > 0 do
      decr top;
      let id = frontier.(!top) in
      for k = start.(id) to start.(id + 1) - 1 do
        let p = preds.(k) in
        if not live.(p)
        then (
          live.(p) <- true;
          frontier.(!top) <- p;
          incr top)
      done
    done;
    live)
;;

(* Drop the dead states and the edges into them. [None] when there are
   none, which is the common case and not worth copying an automaton
   for.

   This stands in for the completion with a sink that Moore's
   algorithm is stated over. Refinement reads the transitions as
   stored, where "no transition on c" and "a transition on c into a
   dead state" are different signatures for the same behaviour, so
   with a dead state present it splits states that are equivalent and
   the dead state and its in-edges survive the pass. Completing with a
   sink reconciles the two, at the price of a state that has to be
   split back out again -- a whole extra refinement pass on inputs
   that would otherwise settle in one. Removing them up front reaches
   the same partition: every state that remains reaches an accepting
   state, so no two of them are separated by a difference only a sink
   could have closed. *)
let trim (dfa : t) : t option =
  let n = dfa.num_states in
  if n = 0
  then None
  else (
    let live = live_states dfa in
    let n_live = Array.fold_left (fun k b -> if b then k + 1 else k) 0 live in
    if n_live = n
    then None
    else if not live.(0)
    then
      (* Nothing reachable accepts anything, so the language is empty
         and its minimal automaton is the one state that goes nowhere.
         Every state merges into it, and [reaches] at a merged state is
         their union. *)
      Some
        { num_states = 1
        ; accepts = [| [] |]
        ; reaches =
            [| List.sort_uniq Int.compare (List.concat (Array.to_list dfa.reaches)) |]
        ; transitions = [| [] |]
        }
    else (
      (* Live states keep their relative order, so the initial state
         stays state 0. *)
      let renumber = Array.make n (-1) in
      let next = ref 0 in
      for id = 0 to n - 1 do
        if live.(id)
        then (
          renumber.(id) <- !next;
          incr next)
      done;
      let accepts = Array.make n_live [] in
      let reaches = Array.make n_live [] in
      let transitions = Array.make n_live [] in
      for id = 0 to n - 1 do
        if live.(id)
        then (
          let k = renumber.(id) in
          accepts.(k) <- dfa.accepts.(id);
          reaches.(k) <- dfa.reaches.(id);
          transitions.(k)
          <- List.filter_map
               (fun (cs, dst) -> if live.(dst) then Some (cs, renumber.(dst)) else None)
               dfa.transitions.(id))
      done;
      Some { num_states = n_live; accepts; reaches; transitions }))
;;

(* The refinement signature, a state's current block plus its
   outgoing transitions coalesced by destination block. Hashes and
   compares through [Ucharset.hash] and [Ucharset.equal], so both
   stay proportional to the charsets involved. *)
module Sig_key = struct
  type t = int * (int * Ucharset.t) list

  let rec equal_trans xs ys =
    match xs, ys with
    | [], [] -> true
    | (b1, c1) :: xt, (b2, c2) :: yt ->
      b1 = b2 && Ucharset.equal c1 c2 && equal_trans xt yt
    | _ -> false
  ;;

  let equal (b1, t1) (b2, t2) = b1 = b2 && equal_trans t1 t2
  let mix h x = h * 31 lxor x land max_int

  let hash (b, ts) =
    List.fold_left
      (fun h (d, c) -> mix (mix h d) (Ucharset.hash c))
      (mix 0x5119_5119 b)
      ts
  ;;
end

module Sig_table = Hashtbl.Make (Sig_key)

let refine (dfa : t) : t =
  let n = dfa.num_states in
  if n <= 1
  then dfa
  else (
    let block_of = Array.make n 0 in
    let num_blocks = ref 0 in
    (* Initial partition: by accepts. *)
    let init_tbl : (int list, int) Hashtbl.t = Hashtbl.create 16 in
    for id = 0 to n - 1 do
      let acc = dfa.accepts.(id) in
      let bid =
        match Hashtbl.find_opt init_tbl acc with
        | Some bid -> bid
        | None ->
          let bid = !num_blocks in
          incr num_blocks;
          Hashtbl.add init_tbl acc bid;
          bid
      in
      block_of.(id) <- bid
    done;
    (* Refine until stable. The signature includes the current
       block to ensure we never merge across the previous partition
       (refinement only splits). *)
    let stable = ref false in
    while not !stable do
      let sig_tbl : int Sig_table.t = Sig_table.create (2 * n) in
      let new_num_blocks = ref 0 in
      let new_block_of = Array.make n 0 in
      for id = 0 to n - 1 do
        (* Coalesce by destination block, so the signature records
           which inputs reach which block: a state with [{a,d} -> S]
           signs the same as one with [{a} -> S], [{d} -> S]. Sort,
           then union each run in one pass. *)
        let trans =
          let by_block =
            List.map (fun (cs, dst) -> block_of.(dst), cs) dfa.transitions.(id)
          in
          let sorted = List.sort (fun (b1, _) (b2, _) -> Int.compare b1 b2) by_block in
          let rec group = function
            | [] -> []
            | (b, c) :: rest ->
              let rec run acc = function
                | (b', c') :: tl when b' = b -> run (c' :: acc) tl
                | tl -> acc, tl
              in
              let cs, tl = run [ c ] rest in
              let u =
                match cs with
                | [ c ] -> c
                | cs -> Ucharset.union_list cs
              in
              (b, u) :: group tl
          in
          group sorted
        in
        let sig_ = block_of.(id), trans in
        let bid =
          match Sig_table.find_opt sig_tbl sig_ with
          | Some bid -> bid
          | None ->
            let bid = !new_num_blocks in
            incr new_num_blocks;
            Sig_table.add sig_tbl sig_ bid;
            bid
        in
        new_block_of.(id) <- bid
      done;
      if !new_num_blocks = !num_blocks
      then stable := true
      else (
        Array.blit new_block_of 0 block_of 0 n;
        num_blocks := !new_num_blocks;
        (* Every state alone in its block, so nothing further can
           split. *)
        if !num_blocks = n then stable := true)
    done;
    let nb = !num_blocks in
    if nb = n
    then dfa (* nothing merged, the construction was already minimal *)
    else (
      (* BFS-renumber blocks starting from the block containing the
         original initial state. Stable, deterministic ordering. *)
      let initial_block = block_of.(0) in
      let block_to_new = Array.make nb (-1) in
      block_to_new.(initial_block) <- 0;
      let next_new = ref 1 in
      (* Find a representative (smallest-id) state for each block,
         once, for use during BFS. *)
      let block_repr = Array.make nb (-1) in
      for id = 0 to n - 1 do
        let bid = block_of.(id) in
        if block_repr.(bid) = -1 then block_repr.(bid) <- id
      done;
      let queue = Queue.create () in
      Queue.add initial_block queue;
      while not (Queue.is_empty queue) do
        let bid = Queue.pop queue in
        let rep = block_repr.(bid) in
        List.iter
          (fun (_, dst) ->
             let dst_bid = block_of.(dst) in
             if block_to_new.(dst_bid) = -1
             then (
               block_to_new.(dst_bid) <- !next_new;
               incr next_new;
               Queue.add dst_bid queue))
          dfa.transitions.(rep)
      done;
      (* Any block the search did not reach is appended. *)
      for bid = 0 to nb - 1 do
        if block_to_new.(bid) = -1
        then (
          block_to_new.(bid) <- !next_new;
          incr next_new)
      done;
      let new_n = nb in
      let new_accepts = Array.make new_n [] in
      let new_reaches = Array.make new_n [] in
      let new_transitions = Array.make new_n [] in
      let seen = Array.make new_n false in
      for id = 0 to n - 1 do
        let new_id = block_to_new.(block_of.(id)) in
        if not seen.(new_id)
        then (
          new_accepts.(new_id) <- dfa.accepts.(id);
          new_transitions.(new_id)
          <- List.map
               (fun (cs, dst) -> cs, block_to_new.(block_of.(dst)))
               dfa.transitions.(id);
          new_reaches.(new_id) <- dfa.reaches.(id);
          seen.(new_id) <- true)
        else
          (* Block already populated by an earlier original state.
             accepts and transition-block-structure must match by
             equivalence; reaches at the merged state is the union. *)
          new_reaches.(new_id)
          <- List.sort_uniq Int.compare (new_reaches.(new_id) @ dfa.reaches.(id))
      done;
      { num_states = new_n
      ; accepts = new_accepts
      ; reaches = new_reaches
      ; transitions = new_transitions
      }))
;;

let minimise (dfa : t) : t =
  refine
    (match trim dfa with
     | Some trimmed -> trimmed
     | None -> dfa)
;;

(* -- emission -----------------------------------------------------------------

   What a code generator needs to emit a lexer, where {!transitions}
   serves a caller inspecting one.

   {!transitions} is one entry per block of the state's own partition,
   which a generator turns into a chain of interval tests. Two views
   sit better with a generator. A generator with a fast path over part
   of the codespace emits that chain on both sides of it, though only
   the arms meeting that part can fire there; {!transitions_in} clips
   to a range so each chain carries what fires. And a chain per state
   is a function per state, where the whole automaton usually
   distinguishes far fewer character classes than it has states (46
   classes over 1739 states for a lexer with 400 keywords); {!table}
   is those classes and one array.
   -------------------------------------------------------------------------- *)

(* Transitions clipped to [lo .. hi], keeping the arms that meet it. A
   generator emitting a dispatch per range asks once per range, and
   emits what fires there. *)
let transitions_in t id ~lo ~hi =
  let window = Ucharset.range ~lo ~hi in
  List.filter_map
    (fun (cs, dst) ->
       let clipped = Ucharset.inter cs window in
       if Ucharset.is_empty clipped then None else Some (clipped, dst))
    t.transitions.(id)
;;

type table =
  { classes : Ucharset.t array
  ; next : int array
  }

(* The classes are the coarsest partition of the codespace that every
   state's transitions respect, so a character's class is all the
   automaton needs to know about it. The complement of a state's
   labels joins the meet as a block of its own, which keeps a
   character with a transition apart from one without.

   [next] is filled by representative, one codepoint per class, since
   a class sits inside one arm or outside them all. That is
   [states * classes] membership tests, the size of the result. *)
let table (t : t) : table =
  let parts = ref [] in
  for id = 0 to t.num_states - 1 do
    let labels = List.map fst t.transitions.(id) in
    let rest = Ucharset.comp (Ucharset.union_list labels) in
    parts := Ucharset.Partition.of_blocks (rest :: labels) :: !parts
  done;
  let joint = Ucharset.Partition.meet_all !parts in
  let k = Ucharset.Partition.num_blocks joint in
  let reps = Array.of_list (Ucharset.Partition.representatives joint) in
  let next = Array.make (t.num_states * k) (-1) in
  for id = 0 to t.num_states - 1 do
    let row = id * k in
    List.iter
      (fun (cs, dst) ->
         for c = 0 to k - 1 do
           if Ucharset.mem cs reps.(c) then next.(row + c) <- dst
         done)
      t.transitions.(id)
  done;
  { classes = Array.of_list (Ucharset.Partition.blocks joint); next }
;;

(* -- pretty-printing ------------------------------------------------------- *)

let pp ppf t =
  Format.fprintf
    ppf
    "@[<v>DFA: %d state%s@,"
    t.num_states
    (if t.num_states = 1 then "" else "s");
  for id = 0 to t.num_states - 1 do
    Format.fprintf ppf "  s%d" id;
    let acc = t.accepts.(id) in
    if acc <> []
    then
      Format.fprintf
        ppf
        " [accepts: %a]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
           Format.pp_print_int)
        acc;
    Format.fprintf ppf "@,";
    List.iter
      (fun (cs, dst) -> Format.fprintf ppf "    %a -> s%d@," Ucharset.pp cs dst)
      t.transitions.(id)
  done;
  Format.fprintf ppf "@]"
;;
