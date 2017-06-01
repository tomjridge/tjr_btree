(** Iterate small-step map operations to provide big-step map
   operations. The important functions in this module are [find],
   [insert], [insert_many] and [delete] (together with the leaf stream
   big-step operations). *)

(* iterate operations till finished ---------------------------------------- *)

(* this includes code to construct initial states (eg init find_state)
   and apply the step functions repeatedly *)

open Base_types
open Prelude
open Monad

module X = Small_step

type 't iter_ops = {
  check_state:'t -> bool;
  check_trans: 't -> 't -> bool;
  step: (unit,'t) m;
  finished: 't -> bool
}

let rec iter ops : (unit,'t) m = (fun s ->
    match ops.finished s with
    | false -> (
        ops.step s|> (fun x ->
            match x with
            | (s',Ok ()) -> (
                assert(ops.check_state s');
                assert(ops.check_trans s s');
                iter ops s')
            | (s',Error e) -> (s',Error(e))))
    | true -> (s,Ok()) )

(*
let if_some f = function Some x -> f x | _ -> ()
*)

(* let check_some f x = (match x with None -> true | Some x -> f x) *)


(* Raw_map exposes a monad, but here we repeatedly run the step
   and check various invariants *)

(* r2t takes a state s:'t and a page ref r:'r and produces an optional
   tree; this is required for checking; if we have it, we also have
   the initial tree *)

type ('o,'t) tt = {
  (* spec_tree:('k,'v)tree option; (* fixed for duration of iter *) *)
  store: 't;
  op_state: 'o;
(*  init_args: 'a; *)
}

open Params2

let mk (type o t) ~check_state ~finished ~dest ~step ~(init_op_state:o) ~(init_store:t) : (t * 'u res) = (
  let check_trans s s' = true in      (* TODO *)
  let step (x:(o,t)tt) : _ * unit res = (
    step x.op_state x.store
    |> (fun (s',y) -> 
        match y with
        | Ok fs' -> ({store=s';op_state=fs'},Ok ())
        | Error e -> ({x with store=s'},Error e) ))
  in
  let ops = { check_state; check_trans; step; finished } in
  iter ops 
  |> run {op_state=init_op_state;store=init_store}
  |> (function 
      | (s,Ok ()) -> (s.store,Ok(dest s))
      | (s,Error e) -> (s.store,Error e)))

let _ = mk


(* find ---------------------------------------- *)


let mk_find (type k v r t) ~cmp ~constants ~store_ops ~(k:k) ~(r:r) ~(init_store:t) = (
  let init_op_state = X.mk_find_state k r in
  let finished s = s.op_state |> X.dest_f_finished |> is_Some in
  let dest s : (r * (k*v)list) = s.op_state |> X.dest_f_finished |> (fun (Some(_,_,r,kvs,_)) -> (r,kvs)) in
  let step = X.find_step ~constants ~cmp ~store_ops in
  let with_check ~r2t ~k2j ~v2j ~r2j = 
    let tree = r2t init_store r |> dest_Some in
    let check_state s = (
      log __LOC__;
      log (tree |> Tree.tree_to_yojson k2j v2j |> Yojson.Safe.pretty_to_string);
      log (s.op_state |> Isa_export.Find.find_state_to_yojson k2j v2j r2j |> Yojson.Safe.pretty_to_string);
      test (fun _ -> assert (X.wellformed_find_state ~r2t ~cmp tree s.store s.op_state));
      true)
    in
    mk ~check_state ~finished ~dest ~step ~init_op_state ~init_store
  in
  let check_state s = true in
  let without_check = 
    mk ~check_state ~finished ~dest ~step ~init_op_state ~init_store
  in
  (with_check,without_check))


(** Find. Take a key, a pointer to the root of a B-tree, and the
    global state. Return the updated state, a reference to the leaf of
    the B-tree that possibly contains the key (or alternatively return
    an error). *)
let find ~cmp ~constants ~store_ops ~k ~r ~init_store = (
  mk_find ~cmp ~constants ~store_ops ~k ~r ~init_store |> snd)

let find_with_check ~cmp ~constants ~store_ops ~k ~r ~init_store ~r2t ~k2j ~v2j ~r2j = (
  mk_find ~cmp ~constants ~store_ops ~k ~r ~init_store |> fst |> fun f -> f ~r2t ~k2j ~v2j ~r2j)


(* insert ---------------------------------------- *)

(** Insert. Take a key, a value, a reference (to the B-tree root) and
   a state and return an updated state, with a new reference to the
   root of the updated B-tree. *)
let insert ~cmp ~constants ~r2t ~tree2j ~op_state2j ~store_ops = (
  let wf_op_state t s = match r2t with None -> true | Some r2t ->
    let (k,v) = s.init_args in
    X.wellformed_insert_state ~cmp ~constants ~r2t t s.store k v s.op_state
  in
  let finished s = s.op_state|>X.dest_i_finished |> is_Some in
  let dest s : 'r = X.dest_i_finished s |> dest_Some in
  let step = X.insert_step ~cmp ~constants ~store_ops in
  let init_op_state (k,v) r = X.mk_insert_state k v r in
  mk ~r2t ~tree2j ~op_state2j ~store_ops ~wf_op_state ~finished ~dest ~step ~init_op_state
  |> fun op -> 
  let f k v r s : ('t * 'r res) = op (k,v) r s in
  f)


(* insert many ---------------------------------------- *)

(** Insert many. Take a key, value, and list of further key/values, a
   pointer to the B-tree root and a global state. Return the updated
   state, a new pointer to a B-tree root, and a list of key/values
   which could not be inserted into the same leaf as the first
   key/value. Typically this function is called in a loop till all kvs
   have been inserted. It is assumed faster than multiple inserts,
   although caching may invalidate this assumption. *)

let insert_many ~cmp ~constants ~r2t ~tree2j ~op_state2j ~store_ops = (
  let wf_op_state t s = true (* TODO *) in
  let finished s = s.op_state |> X.dest_im_finished |> is_Some in
  let dest s : 'r*'kvs = s |> X.dest_im_finished |> dest_Some in
  let step = X.im_step ~constants ~cmp ~store_ops in
  let init_op_state (k,v,kvs) r = X.mk_im_state k v kvs r in
  mk ~r2t ~tree2j ~op_state2j ~store_ops ~wf_op_state ~finished ~dest ~step ~init_op_state
  |> fun op -> 
  let f k v kvs r s : ('t * ('r * 'kv list) res) = op (k,v,kvs) r s in
  f)


(* delete ---------------------------------------- *)

(** Delete. Take a key and a reference to a B-tree root, and a global
   state. Return the updated state and a reference to the new root. *)
let delete ~cmp ~constants ~r2t ~tree2j ~op_state2j ~store_ops = (
  let wf_op_state t s = match r2t with None -> true | Some r2t ->
    let k = s.init_args in
    X.wellformed_delete_state ~cmp ~constants ~r2t t s.store k s.op_state
  in
  let finished s = s.op_state|>X.dest_d_finished|>is_Some in
  let dest s : 'r = X.dest_d_finished s |> (fun (Some(r)) -> r) in
  let step = X.delete_step ~constants ~cmp ~store_ops in
  let init_op_state k r = X.mk_delete_state k r in
  mk ~r2t ~tree2j ~store_ops ~wf_op_state ~finished ~dest ~step ~init_op_state ~op_state2j
  |> fun op -> 
  let f k r s : ('t * 'r res) = op k r s in
  f)


(* make pre_map_ops ---------------------------------------- *)

open Pre_map_ops

(** Construct [pre_map_ops] using functions above. Takes a "parameters" object ps. *)
let make_pre_map_ops ~cmp ~constants ~r2t ~tree2j ~f2j ~i2j ~im2j ~d2j ~store_ops = 
  {
    find=(find ~cmp ~constants ~r2t ~tree2j ~op_state2j:f2j ~store_ops);
    insert=(insert ~cmp ~constants ~r2t ~tree2j ~op_state2j:i2j ~store_ops);
    insert_many=(insert_many ~cmp ~constants ~r2t ~tree2j ~op_state2j:im2j ~store_ops);
    delete=(delete ~cmp ~constants ~r2t ~tree2j ~op_state2j:d2j ~store_ops);
  }

let _ = make_pre_map_ops



