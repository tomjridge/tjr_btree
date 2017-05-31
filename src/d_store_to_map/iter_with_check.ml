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

let check_some f x = (match x with None -> true | Some x -> f x)


(* find ---------------------------------------- *)

(* Raw_map exposes a monad, but here we repeatedly run the step
   and check various invariants *)

(* r2t takes a state s:'t and a page ref r:'r and produces an optional
   tree; this is required for checking; if we have it, we also have
   the initial tree *)

type ('k,'v,'t,'o,'a) t = {
  spec_tree:('k,'v)tree option; (* fixed for duration of iter *)
  store: 't;
  op_state: 'o;
  init_args: 'a;
}

open Bt_params2

(* dbg_ps takes and fun ... -> () and calls fun with params or does nothign if no dbg *)
let mk ~cmp ~constants ~tree2j ~r2t ~store_ops ~op_state2j
    ~wf_op_state ~finished ~dest ~step ~init_op_state
  = (

    let check_state s = (
      (match s.spec_tree with | None -> () | Some t ->
          log __LOC__;
          log (t |> tree2j |> Yojson.Safe.pretty_to_string));
      (match op_state2j with | None -> () | Some op_state2j ->
          log __LOC__;
          log (s.op_state |> op_state2j |> Yojson.Safe.pretty_to_string));
      (match s.spec_tree with | None -> () | Some t ->
          test (fun _ -> assert (wf_op_state t s)));
      true)
    in
    let check_trans s s' = true in      (* TODO *)
    let dest s = dest s.op_state in

    (* NB this does not update tree - that needs to be passed in at the
       start of the iter *)
    let step x : _ * unit res = (
      step x.op_state x.store
      |> (fun (s',y) -> 
          match y with
          | Ok fs' -> ({ x with store=s';op_state=fs'},Ok ())
          | Error e -> ({x with store=s'},Error e) ))
    in
    let ops = { check_state; check_trans; step; finished } in
    let mk_init_state init_args r s = 
      let spec_tree = match r2t with None -> None | Some r2t -> r2t s r in
      {
        spec_tree;
        store=s; 
        op_state=init_op_state init_args r;
        init_args;
      }
    in
    let op init_args r s : ('t * 'u res) = (
      mk_init_state init_args r s 
      |> (fun s -> iter ops |> run s)
      |> (function 
          | (s,Ok ()) -> (s.store,Ok(dest s))
          | (s,Error e) -> (s.store,Error e)))
    in
    op)


(** Find. Take a key, a pointer to the root of a B-tree, and the
    global state. Return the updated state, a reference to the leaf of
    the B-tree that possibly contains the key (or alternatively return
    an error). *)
let find ~cmp ~constants ~r2t ~tree2j ~op_state2j ~store_ops = (
  let wf_op_state t s = match r2t with None -> true | Some r2t -> 
    X.wellformed_find_state ~cmp ~r2t t s.store s.op_state in
  let finished s = s.op_state|>X.dest_f_finished|>(fun x -> x<>None) in
  let dest s : ('r * 'kvs) = X.dest_f_finished s |> (fun (Some(_,_,r,kvs,_)) -> (r,kvs)) in
  let step = X.find_step ~constants ~cmp ~store_ops in
  let init_op_state k r = X.mk_find_state k r in
  mk ~cmp ~constants ~r2t ~tree2j ~store_ops ~wf_op_state ~finished ~dest ~step ~init_op_state ~op_state2j
  |> fun op -> 
  let f k r s : ('t * ('r*'kvs) res) = op k r s in
  f)


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
  mk ~cmp ~constants ~r2t ~tree2j ~op_state2j ~store_ops ~wf_op_state ~finished ~dest ~step ~init_op_state
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
  mk~cmp ~constants ~r2t ~tree2j ~op_state2j ~store_ops ~wf_op_state ~finished ~dest ~step ~init_op_state
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
  mk ~cmp ~constants ~r2t ~tree2j ~store_ops ~wf_op_state ~finished ~dest ~step ~init_op_state ~op_state2j
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



