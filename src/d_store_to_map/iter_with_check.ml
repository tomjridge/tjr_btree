(* iterate operations till finished ---------------------------------------- *)

(* this includes code to construct initial states (eg init find_state)
   and apply the step functions repeatedly *)

open Prelude

module IU = Isa_util

type ('t,'f) iter_ops = {
  check_state:'t -> bool;
  check_trans: 't -> 't -> bool;
  step: 't -> ('t * (unit,string) result);
  dest: 't -> 'f option
}

let rec iter ops : 't -> 't*('f,string)result = (fun s ->
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
let map_option f = function Some x -> Some (f x) | _ -> None


(* find ---------------------------------------- *)

(* Raw_map exposes a monad, but here we repeatedly run the step
   and check various invariants *)
module Find = (struct 
  type ('k,'v,'r,'t) t = {
    tree: ('k,'v) Tree.tree option; (* NB for testing *)
    store: 't;
    fs: ('k,'v,'r) IU.find_state;
    ps1: ('k,'v,'r,'t) IU.Params_.ps1;
    r2f: 't -> ('k,'v,'r) IU.r2f
  }

  type ('k,'v,'r) finished = 'r * ('k * 'v) list
end)

open Find

let check_state s= (
  Test.log __LOC__;
  (* Test.log (s.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
  Test.test (
    fun _ ->
      s.tree |> if_some (fun t -> 
          assert (IU.wellformed_find_state s.ps1.ps0 (s.r2f s.store) t s.fs)));
  true
)

let check_trans s s' = true       (* TODO *)
  
let dest: 't -> ('k,'v,'r) finished option = fun s -> 
  s.fs|>IU.dest_f_finished|>(map_option (fun (_,_,r,kvs,_) -> (r,kvs)))

(* NB this does not update tree - that needs to be passed in every time *)
let step : 't -> ('t * (unit,string)result) = (fun x ->
    IU.find_step x.ps1 x.fs x.store
    |> (fun (s',y) -> 
        match y with
        | Ok fs' -> ({ x with store=s';fs=fs'},Ok ())
        | Error e -> ({x with store=s'},Error e)
      ))

let find_ops = { check_state; check_trans; step; dest } 

let mk ps1 r2f t k r s = {
    tree=t; 
    store=s; 
    fs=IU.mk_find_state k r;
    ps1=ps1;
    r2f=r2f
  }

let find ps1 r2f t k r s : 't * ('r*('k*'v)list,string) result = (
  mk ps1 r2f t k r s 
  |> iter find_ops 
  |> (fun (s,r) -> (s.store,r)))



(* insert ---------------------------------------- *)

module Insert = (struct  
  type ('k,'v,'r,'t) t = {
    tree: ('k,'v) Tree.tree option;
      k:'k;
      v:'v;
      store: 't;
      is: ('k,'v,'r) IU.insert_state;
      ps1: ('k,'v,'r,'t) IU.Params_.ps1;
      r2f: 't -> ('k,'v,'r) IU.r2f
    }

  type 'r finished = 'r
end)

open Insert

let check_state s = (
(*
          Test.log __LOC__;
          Test.log ("s.t" ^ (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string));
          Test.log ("s.k" ^ (s.k |> KV_.key_to_yojson |> Yojson.Safe.to_string));
          Test.log ("s.v" ^ (s.v |> KV_.value_t_to_yojson |> Yojson.Safe.to_string));
          Test.log ("s.is" ^ (s.is |> Insert.i_state_t_to_yojson |> Yojson.Safe.to_string));
*)
  Test.test (fun _ ->
      s.tree |> if_some (fun t ->
          assert (IU.wellformed_insert_state s.ps1.ps0 (s.r2f s.store) t s.k s.v s.is)));
  true
)

let check_trans x y = (true)

let dest: 't -> 'r finished option = 
  fun s -> s.is |> IU.dest_i_finished

let step : 't -> ('t* (unit,string)result) = (fun s ->
    IU.insert_step s.ps1 s.is s.store 
    |> (fun (s',y) -> 
        match y with
        | Ok is' -> ({ s with store=s';is=is'},Ok ())
        | Error e -> ({ s with store=s'},Error e)
      ))

let insert_ops = { check_state; check_trans; step; dest } 

let mk ps1 r2f t k v r s = {  
  tree=t; 
  k; v; 
  store=s; 
  is=(IU.mk_insert_state k v r);
  ps1;
  r2f;
}

let insert ps1 r2f t k v r s = (
  mk ps1 r2f t k v r s 
  |> iter insert_ops
  |> (fun (s,r) -> (s.store,r)))



(* insert many ---------------------------------------- *)

module Im = (struct  
  type ('k,'v,'r,'t) t = {
    tree: ('k,'v) Tree.tree option;
      k:'k;
      v:'v;
      kvs: ('k*'v) list;
      store: 't;
      is: ('k,'v,'r) IU.im_state;
      ps1: ('k,'v,'r,'t) IU.Params_.ps1;
      r2f: 't -> ('k,'v,'r) IU.r2f
    }

  type 'r finished = 'r
end)

open Im

let check_state s = true (* TODO *)

let check_trans x y = true

let dest: 't -> 'r finished option = 
  fun s -> s.is |> IU.dest_im_finished

let step : 't -> ('t* (unit,string)result) = (fun s ->
    IU.im_step s.ps1 s.is s.store 
    |> (fun (s',y) -> 
        match y with
        | Ok is' -> ({ s with store=s';is=is'},Ok ())
        | Error e -> ({ s with store=s'},Error e)
      ))

let im_ops = { check_state; check_trans; step; dest } 

let mk ps1 r2f t k v kvs r s = {  
  tree=t; 
  k; v; kvs;
  store=s; 
  is=(IU.mk_im_state k v kvs r);
  ps1;
  r2f;
}

let insert_many ps1 r2f t k v kvs r s = (
  mk ps1 r2f t k v kvs r s 
  |> iter im_ops
  |> (fun (s,r) -> (s.store,r)))



(* delete ---------------------------------------- *)

module Delete = (struct 
  type ('k,'v,'r,'t) t = {
    tree:('k,'v)Tree.tree option;
    k:'k;
    store:'t;
    ds: ('k,'v,'r) IU.delete_state;
    ps1: ('k,'v,'r,'t) IU.Params_.ps1;
    r2f: 't -> ('k,'v,'r) IU.r2f
  }

  type 'r finished = 'r
end)

open Delete

let check_state s = (
  Test.log __LOC__;
  (* Test.log (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
  Test.test (fun _ -> 
      s.tree |> if_some (fun t -> 
          assert (IU.wellformed_delete_state s.ps1.ps0 (s.r2f s.store) t s.k s.ds)));
  true
)

let check_trans x y = (true)

let dest: 't -> 'r finished option = fun s -> s.ds |> IU.dest_d_finished

let step : 't -> ('t*(unit,string)result) = (fun x ->
    IU.delete_step x.ps1 x.ds x.store
    |> (fun (s',y) -> 
        match y with
        | Ok ds' -> ({ x with store=s';ds=ds'},Ok ())
        | Error e -> ({x with store=s'},Error e)
      ))
  
let delete_ops = { check_state; check_trans; step; dest } 

let mk ps1 r2f t k r s = {
  tree=t; 
  k;
  store=s; 
  ds=IU.mk_delete_state k r;
  ps1;
  r2f
}

let delete ps1 r2f t k r s : ('t * ('r option,string)result) = (
  mk ps1 r2f t k r s 
  |> iter delete_ops 
  |> (fun (s,r) -> (s.store,r)))



