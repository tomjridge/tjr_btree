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



(* leaf stream ---------------------------------------- *)

module Leaf_stream_ = (struct 
  (* we need to repeatedly step the leaf state to the point that
     we hit a leaf and dest_LS_leaf <> None; INVARIANT every
     ls_state constructed or exposed here has dest_LS_leaf <>
     None *)
  type ls_state = IU.Leaf_stream.ls_state
  let rec next_leaf: ls_state -> ls_state option m = Simple_monad.(fun s ->
      match (IU.Leaf_stream.lss_is_finished s) with
      | true -> return None
      | false -> (
          s |> IU.Leaf_stream.lss_step |> bind (fun s' ->
              match (IU.Leaf_stream.dest_LS_leaf s') with
              | None -> next_leaf s'
              | Some _ -> return (Some s'))))
  let mk_leaf_stream : P.page_ref -> ls_state m = Simple_monad.(fun r ->
      IU.Leaf_stream.mk_ls_state r |> (fun s ->
          match (IU.Leaf_stream.dest_LS_leaf s) with
          | None -> (next_leaf s |> bind (fun s -> return (dest_Some s)))
          | Some _ -> return s))
  let ls_kvs: ls_state -> (k*v) list = (fun s ->
      s |> IU.Leaf_stream.dest_LS_leaf |> dest_Some)  (* guaranteed <> None *)
  let ls_step: ls_state -> ls_state option m = next_leaf
end)



(* FIXME remove this? *)
(* include ops in top level ------------------------------------------ *)

module R = struct
  module P = P
  let find = Find_.find
  let insert = Insert_.insert 
  let delete = Delete_.delete
  let insert_many = Insert_many_.insert_many
  let mk_leaf_stream = Leaf_stream_.mk_leaf_stream
  let ls_step = Leaf_stream_.ls_step
  let ls_kvs = Leaf_stream_.ls_kvs
end

let _ = (module R : RR)

include R
end) (* Make *)
end) (* Make_functor *)






(* old ============================================================ *)


(*
(* FIXME do we need something that takes a disk (with World.m) and produces a map (with World.m)? or a Poly.store to a Poly.store with World.m? *)


(* if we have access to some state passing map, with arbitrary impl type 't *)
module Map_final = struct

  type ('k,'v,'t) pre_map = {
    find: 'k -> 't -> ('t * ('v option,string)result)
  }

  type ('k,'v) map = WV: ('k,'v,'t) pre_map -> ('k,'v) map

  module type Ops = sig
    val find: ('k,'v) map -> 'k -> 'v option World.m
  end

end
 *)

(* poly, with the world monad ---------------------------------------- *)

(*
module Poly_world = (struct

  type page_ref = int
  type ('k,'v) frame = ('k,'v,page_ref) Frame.t

  (* type ('a,'store) m = 'store -> ('store * ('a,string)result) *)

(*                                 
  type ('k,'v,'store) t = {
    compare_k: 'k -> 'k -> int;
    equal_v: 'v -> 'v -> bool;
    cs0: Isa_util.constants;
    store_free: page_ref list -> (unit,'store)  m;
    store_read : page_ref -> (('k, 'v) frame,'store) m;
    store_alloc : ('k, 'v) frame -> (page_ref,'store) m;
    mk_r2f: 'store -> page_ref -> ('k,'v) frame option;
  }
*)

  type 'store t = ('store*page_ref) World.r

  type ('k,'v,'store) pre_map = {
    find: 'store t -> 'k -> (page_ref * ('k*'v) list) World.m
  }

  (*
  type ('k,'v,'t) map' = {
    find: 't -> 'k -> (page_ref * ('k*'v) list) World.m
  }
  *)

  (* FIXME could use this type instead of exposing 'store above *)
  type ('k,'v) tt = T': ('k,'v,'store) pre_map * 'store * page_ref -> ('k,'v) tt


  let make = (fun (poly:('k,'v,'store)Poly.map) -> 
      let find: 'store t -> 'k -> (page_ref * ('k*'v) list) World.m = (
        fun wr k -> World.(
            get wr |> bind (fun (s,r) -> 
                poly.find k r s |> (fun (s',res) ->                    
                        match res with
                        | Ok (r,kvs) -> (
                            set wr (s',r) |> bind (fun () -> return (r,kvs)))
                        | Error e -> Sem.err e))))
      in
      {find})


end)
*)

(* map-like interface ---------------------------------------- *)

(*
module Map = (struct

  open Poly_world

  (* FIXME use Poly_world.pre_map? remove Poly_world? *)
  type ('k,'v,'store) pre_wv = {
    r:page_ref;
    s:'store;
    find: 'k->page_ref->'store->'store*(page_ref * ('k*'v) list,string)result
  }

  type ('k,'v) world_value = WV: ('k,'v,'store) pre_wv -> ('k,'v) world_value

  type ('k,'v) map = ('k,'v) world_value World.r

  (* this is an interface that mimics the standard map interface *)
  let find: ('k,'v) map -> 'k -> (page_ref * ('k*'v) list) World.m = (
    fun m k -> World.(
        get m |> bind (function WV wv -> (
              (* 'store existential in following *)
              (* let wv = ((Obj.magic wv) : ('k,'v,'store) pre_wv) in *)
              wv.find k wv.r wv.s |> (fun (s',res) -> 
                  match res with
                  | Ok (r,kvs) -> (
                      set m (WV {wv with s=s'; r=r}) 
                      |> bind (fun () -> return (r,kvs)))
                  | Error e -> (
                      set m (WV {wv with s=s'})
                      |> bind (fun () -> Sem.err e)))))))

end)
*)





(* if we have access to a map using a world *)
(*
module Map_final' = struct

  type ('k,'v,'wr) pre_map = {
    find: 'wr -> 'k -> 'v option World.m
  }

  type ('k,'v) wv = WV: ('k,'v,'t) pre_map -> ('k,'v) wv

  type ('k,'v) map = ('k,'v) wv World.r

  module type Ops = sig
    val find: ('k,'v) map -> 'k -> 'v option World.m
  end

end
*)

(* making ref type explicit *)

(*
module Map_final' = struct

  type ('k,'v,'r,'t) pre_map = {
    find: 'r -> 'k -> 't -> 't * ('v option,string) result
  }

  type ('k,'v) wv = WV: ('k,'v,'r,'t) pre_map -> ('k,'v) wv

  type ('k,'v) map = ('k,'v) wv World.r

  module type Ops = sig
    val find: ('k,'v) map -> 'k -> 't -> 't * ('v option,string)result
  end

end





(* don't use this directly - use the record version below *)
module Make_functor = (struct 

  (* used in store_to_map.ml *)
  module type RR = sig
    open Isa_export.Pre_params
    module P : Isa_util.PARAMS
    open P
    val find: (k,v)Tree.tree option -> 
      k -> page_ref -> store -> store * (page_ref*(k*v)list,string) result
    val insert: (k,v)Tree.tree option -> 
      k -> v -> page_ref -> store -> store * (page_ref,string)result
    val insert_many: (k,v)Tree.tree option -> 
      k -> v -> (k*v)list -> 
      page_ref -> store-> store*(page_ref*(k*v)list,string)result
    val delete: (k,v)Tree.tree option -> 
      k -> page_ref -> store -> store * (page_ref,string)result
    val mk_leaf_stream: 
      page_ref -> store -> store * ((k,v,page_ref)ls_state,string)result
    val ls_step: (k,v,page_ref)ls_state -> 
      store -> store * ((k,v,page_ref)ls_state option,string)result
    val ls_kvs:  (k,v,page_ref)ls_state -> (k*v)list
  end

  module Make = functor (P:Isa_util.PARAMS) -> (struct
      module P_ = P
      open P

      module IU = Isa_util.Make(P)
      open IU



*)



