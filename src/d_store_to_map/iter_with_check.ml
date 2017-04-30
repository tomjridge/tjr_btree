(* iterate operations till finished ---------------------------------------- *)

(* this includes code to construct initial states (eg init find_state)
   and apply the step functions repeatedly *)

open Prelude

module IU = Isa_util

type ('t,'f) iter_ops = {
  check_state:'t -> bool;
  check_trans: 't -> 't -> bool;
  step: (unit,'t) m;
  dest: 't -> 'f option
}

let rec iter ops : ('f,'t) m = (fun s ->
    match ops.dest s with
    | None -> (
        ops.step s|> (fun x ->
            match x with
            | (s',Ok ()) -> (
                assert(ops.check_state s');
                assert(ops.check_trans s s');
                iter ops s')
            | (s',Error e) -> (s',Error(e))))
    | Some x -> (s,Ok(x)) )

let if_some f = function Some x -> f x | _ -> ()

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
  type ('k,'v,'r) finished = 'r * ('k * 'v) list
end)

open Find

let check_state s = (
  s.spec_tree 
  |> check_some (fun t ->
      log __LOC__;
      (* log (s.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
      test (fun _ -> assert (IU.wellformed_find_state s.ps t s.store s.fs));
      true))

let check_trans s s' = true       (* TODO *)
  
let dest s : ('k,'v,'r) finished option = 
  s.fs|>IU.dest_f_finished|>(option_map (fun (_,_,r,kvs,_) -> (r,kvs)))

(* NB this does not update tree - that needs to be passed in at the
   start of the iter *)
let step x : 't * unit res = (
  IU.find_step x.ps x.fs x.store
  |> (fun (s',y) -> 
      match y with
      | Ok fs' -> ({ x with store=s';fs=fs'},Ok ())
      | Error e -> ({x with store=s'},Error e)
    ))

let find_ops = { check_state; check_trans; step; dest } 

let mk ps k r s = 
  let spec_tree = r2t ps |> option_map (fun r2t -> r2t s r |> dest_Some) in
  {
    spec_tree; 
    store=s; 
    fs=IU.mk_find_state k r;
    ps=ps;
  }

let find ps k r s : ('t * ('r*'kvs) res) = (
  mk ps k r s 
  |> iter find_ops 
  |> (fun (s,r) -> (s.store,r)))


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
  type 'r finished = 'r
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
      test (fun _ -> assert (IU.wellformed_insert_state s.ps t s.store s.k s.v s.is));
      true))

let check_trans x y = (true)

let dest s : 'r finished option = s.is |> IU.dest_i_finished

let step s : 't * unit res = (
  IU.insert_step s.ps s.is s.store 
  |> (fun (s',y) -> 
      match y with
      | Ok is' -> ({ s with store=s';is=is'},Ok ())
      | Error e -> ({ s with store=s'},Error e) ))

let insert_ops = { check_state; check_trans; step; dest } 

let mk ps k v r s = 
  let spec_tree = r2t ps |> option_map (fun r2t -> r2t s r |> dest_Some) in
  {  
    spec_tree; 
    k; v; store=s; 
    is=(IU.mk_insert_state k v r);
    ps;
  }

let insert ps k v r s = (
  mk ps k v r s 
  |> iter insert_ops
  |> (fun (s,r) -> (s.store,r)))



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
  type 'r finished = 'r
end)

open Im

let check_state s = true (* TODO *)

let check_trans x y = true

let dest: 't -> 'r finished option = 
  fun s -> s.is |> IU.dest_im_finished

let step s : 't * unit res = (
  IU.im_step s.ps s.is s.store 
  |> (fun (s',y) -> 
      match y with
      | Ok is' -> ({ s with store=s';is=is'},Ok ())
      | Error e -> ({ s with store=s'},Error e)
    ))

let im_ops = { check_state; check_trans; step; dest } 

let mk ps k v kvs r s = 
  let spec_tree = r2t ps |> option_map (fun r2t -> r2t s r |> dest_Some) in
  {  
    spec_tree; 
    k; v; kvs;
    store=s; 
    is=(IU.mk_im_state k v kvs r);
    ps;
  }

let insert_many ps k v kvs r s = (
  mk ps k v kvs r s 
  |> iter im_ops
  |> (fun (s,r) -> (s.store,r)))



(* delete ---------------------------------------- *)

module Delete = (struct 
  type ('k,'v,'r,'t,'ps) t = {
    spec_tree: ('k,'v)tree option;
    k:'k;
    store:'t;
    ds: ('k,'v,'r) delete_state;
    ps: 'ps;
  }
  type 'r finished = 'r
end)

open Delete

let check_state s = (
  s.spec_tree
  |> check_some (fun t -> 
      log __LOC__;
      (* log (t |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
      test (fun _ -> assert (IU.wellformed_delete_state s.ps t s.store s.k s.ds));
      true))

let check_trans x y = (true)

let dest s : 'r finished option = s.ds |> IU.dest_d_finished

let step x : 't * unit res = (
  IU.delete_step x.ps x.ds x.store
  |> (fun (s',y) -> 
      match y with
      | Ok ds' -> ({ x with store=s';ds=ds'},Ok ())
      | Error e -> ({x with store=s'},Error e)
    ))

let delete_ops = { check_state; check_trans; step; dest } 

let mk ps k r s = 
  let spec_tree = r2t ps |> option_map (fun r2t -> r2t s r |> dest_Some) in
  {
    spec_tree;
    k;
    store=s; 
    ds=IU.mk_delete_state k r;
    ps;
  }

let delete ps k r s : 't * 'r res = (
  mk ps k r s 
  |> iter delete_ops 
  |> (fun (s,r) -> (s.store,r)))



