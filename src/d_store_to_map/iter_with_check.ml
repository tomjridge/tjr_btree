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

module Find = (struct 
  type ('k,'v,'r,'t,'ps) t = {
    spec_tree:('k,'v)tree option; (* fixed for duration of iter *)
    store: 't;
    fs: ('k,'v,'r) find_state;
    ps: 'ps;
  }
end)

open Find

let check_state s = (
  s.spec_tree 
  |> check_some (fun t ->
      log __LOC__;
      (* log (s.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
      test (fun _ -> assert (X.wellformed_find_state s.ps t s.store s.fs));
      true))

let check_trans s s' = true       (* TODO *)
  
let finished s = s.fs|>X.dest_f_finished|>(fun x -> x<>None)

let dest s : ('r * 'kvs) = 
  s.fs
  |> X.dest_f_finished
  |> function Some (_,_,r,kvs,_) -> (r,kvs)

(* NB this does not update tree - that needs to be passed in at the
   start of the iter *)
let step x : _ * unit res = (
  X.find_step x.ps x.fs x.store
  |> (fun (s',y) -> 
      match y with
      | Ok fs' -> ({ x with store=s';fs=fs'},Ok ())
      | Error e -> ({x with store=s'},Error e) ))

let find_ops = { check_state; check_trans; step; finished } 

let mk ps k r s = 
  let spec_tree = debug ps |> option_map (fun d -> d.r2t s r |> dest_Some) in
  {
    spec_tree; 
    store=s; 
    fs=X.mk_find_state k r;
    ps=ps;
  }

(** Find. Take a key, a pointer to the root of a B-tree, and the
   global state. Return the updated state, a reference to the leaf of
   the B-tree that possibly contains the key (or alternatively return
   an error). *)
let find ps k r s : ('t * ('r*'kvs) res) = (
  mk ps k r s 
  |> (fun s -> iter find_ops |> run s)
  |> (function 
      | (s,Ok ()) -> (s.store,Ok(dest s))
      | (s,Error e) -> (s.store,Error e)))


(* insert ---------------------------------------- *)

module Insert = (struct  
  type ('k,'v,'r,'t,'ps) t = {
    spec_tree: ('k,'v)tree option;
    k:'k;
    v:'v;
    store: 't;
    is: ('k,'v,'r) insert_state;
    ps: 'ps;
  }
end)

open Insert


(*
          log ("s.t" ^ (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string));
          log ("s.k" ^ (s.k |> KV_.key_to_yojson |> Yojson.Safe.to_string));
          log ("s.v" ^ (s.v |> KV_.value_t_to_yojson |> Yojson.Safe.to_string));
          log ("s.is" ^ (s.is |> Insert.i_state_t_to_yojson |> Yojson.Safe.to_string));
*)

let check_state s = (
  s.spec_tree
  |> check_some (fun t -> 
      log __LOC__;
      test (fun _ -> assert (X.wellformed_insert_state s.ps t s.store s.k s.v s.is));
      true))

let check_trans x y = (true)

let finished s = s.is |> X.dest_i_finished |> is_Some

let dest s : 'r = 
  s.is 
  |> X.dest_i_finished 
  |> function Some r -> r

let step s : _ * unit res = (
  X.insert_step s.ps s.is s.store 
  |> (fun (s',y) -> 
      match y with
      | Ok is' -> ({ s with store=s';is=is'},Ok ())
      | Error e -> ({ s with store=s'},Error e) ))

let insert_ops = { check_state; check_trans; step; finished } 

let mk ps k v r s = 
  let spec_tree = debug ps |> option_map (fun d -> d.r2t s r |> dest_Some) in
  {  
    spec_tree; 
    k; v; store=s; 
    is=(X.mk_insert_state k v r);
    ps;
  }

(** Insert. Take a key, a value, a reference (to the B-tree root) and
   a state and return an updated state, with a new reference to the
   root of the updated B-tree. *)
let insert ps k v r s : 't * 'r res = (
  mk ps k v r s 
  |> (fun s -> iter insert_ops |> run s)
  |> (function 
      | (s,Ok ()) -> (s.store,Ok(dest s))
      | (s,Error e) -> (s.store,Error e)))



(* insert many ---------------------------------------- *)

module Im = (struct  
  type ('k,'v,'r,'t,'ps) t = {
    spec_tree: ('k,'v)tree option;
    k:'k;
    v:'v;
    kvs: ('k*'v) list;
    store: 't;
    is: ('k,'v,'r) im_state;
    ps: 'ps;
  }
end)

open Im

let check_state s = true (* TODO *)

let check_trans x y = true

let finished s = s.is |> X.dest_im_finished |> is_Some

let dest s : 'r*'kvs = s.is |> X.dest_im_finished |> dest_Some

let step s : _ * unit res = (
  X.im_step s.ps s.is s.store 
  |> (fun (s',y) -> 
      match y with
      | Ok is' -> ({ s with store=s';is=is'},Ok ())
      | Error e -> ({ s with store=s'},Error e) ))

let im_ops = { check_state; check_trans; step; finished } 

let mk ps k v kvs r s = 
  let spec_tree = debug ps |> option_map (fun d -> d.r2t s r |> dest_Some) in
  {  
    spec_tree; 
    k; v; kvs;
    store=s; 
    is=(X.mk_im_state k v kvs r);
    ps;
  }

(** Insert many. Take a key, value, and list of further key/values, a
   pointer to the B-tree root and a global state. Return the updated
   state, a new pointer to a B-tree root, and a list of key/values
   which could not be inserted into the same leaf as the first
   key/value. Typically this function is called in a loop till all kvs
   have been inserted. It is assumed faster than multiple inserts,
   although caching may invalidate this assumption. *)
let insert_many ps k v kvs r s : 't * ('r*'kvs) res = (
  mk ps k v kvs r s 
  |> (fun s -> iter im_ops |> run s)
  |> (function 
      | (s,Ok ()) -> (s.store,Ok(dest s))
      | (s,Error e) -> (s.store,Error e)))


(* delete ---------------------------------------- *)

module Delete = (struct 
  type ('k,'v,'r,'t,'ps) t = {
    spec_tree: ('k,'v)tree option;
    k:'k;
    store:'t;
    ds: ('k,'v,'r) delete_state;
    ps: 'ps;
  }
end)

open Delete

(* TODO make other checks look like this *)
let check_state s = (
  debug s.ps 
  |> check_some (fun d -> 
      (* INVARIANT debug <> None --> spec_tree <> None *)
      let spec_tree = s.spec_tree |> dest_Some in
      log __LOC__;
      log (spec_tree
           |> Tree.tree_to_yojson (d.k2j) (d.v2j) 
           |> Yojson.Safe.pretty_to_string); 
      log (s.ds
           |> Isa_export.Delete.delete_state_to_yojson d.k2j d.v2j d.r2j
           |> Yojson.Safe.pretty_to_string);
      test (fun _ -> assert (X.wellformed_delete_state 
                               s.ps spec_tree s.store s.k s.ds));
      true))

let check_trans x y = (true)

let finished s = s.ds |> X.dest_d_finished |> is_Some

let dest s : 'r = s.ds |> X.dest_d_finished |> dest_Some

let step x : _ * unit res = (
  X.delete_step x.ps x.ds x.store
  |> (fun (s',y) -> 
      match y with
      | Ok ds' -> ({ x with store=s';ds=ds'},Ok ())
      | Error e -> ({x with store=s'},Error e)  ))

let delete_ops = { check_state; check_trans; step; finished } 

let mk ps k r s = 
  let spec_tree = debug ps |> option_map (fun d -> d.r2t s r |> dest_Some) in
  {
    spec_tree;
    k;
    store=s; 
    ds=X.mk_delete_state k r;
    ps;
  }

(** Delete. Take a key and a reference to a B-tree root, and a global
   state. Return the updated state and a reference to the new root. *)
let delete ps k r s : _ * 'r res = (
  mk ps k r s 
  |> (fun s -> iter delete_ops |> run s)
  |> (function 
      | (s,Ok ()) -> (s.store,Ok(dest s))
      | (s,Error e) -> (s.store,Error e)))

(* make pre_map_ops ---------------------------------------- *)

open Pre_map_ops

(** Construct [pre_map_ops] using functions above. Takes a "parameters" object ps. *)
let make_pre_map_ops ps = 
  {
  find=(find ps);
  insert=(insert ps);
  insert_many=(insert_many ps);
  delete=(delete ps);
}
