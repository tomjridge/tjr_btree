module IE = Isa_export
open IE
open Frame
open Tree

type 'a res = 'a Util.res

type ('a,'t) m = 't -> 't * 'a Util.res

type ('k,'r) rstk = ('k,'r,unit) Tree_stack.ts_frame_ext list
type ('k,'v,'r) r2f = ('r -> ('k,'v,'r) frame option)

type ('k,'v,'r) find_state = ('k,'v,'r) IE.Find.find_state
type ('k,'v,'r) insert_state = ('k,'v,'r) IE.Insert.insert_state
type ('k,'v,'r) im_state = ('k,'v,'r) IE.Insert_many.ist
type ('k,'v,'r) delete_state = ('k,'v,'r) IE.Delete.delete_state
type ('k,'v,'r) ls_state = ('k,'v,'r) IE.Leaf_stream.ls_state

module Params_ = struct
  type 'k ps0 = { 
    compare_k: 'k -> 'k -> int; 
    constants: Constants.t 
  }
  (* TODO make these match up with store_ops *)
  type ('k,'v,'r,'t) ps1 = { 
    ps0: 'k ps0; 
    store_read: 'r -> 't -> ('t * ('k,'v,'r) frame res);
    store_free: 'r list -> 't -> ('t * unit res);
    store_alloc: ('k,'v,'r) frame -> 't -> ('t * 'r res);
  }
end

module X = struct
  let int_to_nat x = Isa_export.(x |>Big_int.big_int_of_int|>Arith.nat_of_integer)
  let int_to_int x = Isa_export.(
      x |>Big_int.big_int_of_int|>(fun x -> Arith.Int_of_integer x))

  open Constants
  let x_constants cs0 : unit IE.Prelude.constants_ext = IE.Prelude.Constants_ext(
      cs0.min_leaf_size|>int_to_nat,
      cs0.max_leaf_size|>int_to_nat,
      cs0.min_node_keys|>int_to_nat,
      cs0.max_node_keys|>int_to_nat,
      ())

  open IE.Params
  open Params_
  let x_cmp cmp x y = cmp x y |> int_to_int
  let x_ps0 ps0 : 'k IE.Params.ps0 = (
    Ps0(
      x_cmp ps0.compare_k,
      ps0.constants|>x_constants))

  let x_ps1 ps1 : ('k,'v,'r,'t) IE.Params.ps1 = IE.Params.(
      let open Params_ in
      let r = ps1.store_read in
      let a = ps1.store_alloc in
      let f = ps1.store_free in
      Ps1(ps1.ps0|>x_ps0,
          (r,(a,f))))
  
end

open Params_

let x5 (x,(y,(z,(w,u)))) = (x,y,z,w,u)

(* find ---------------------------------------- *)

let  mk_find_state: 'k -> 'r -> ('k,'v,'r) find_state = Find.mk_find_state

let find_step: ('k,'v,'r,'t) ps1 -> ('k,'v,'r) find_state -> 't -> 
      't * ('k,'v,'r) find_state res = 
  (fun ps1 fs -> Find.find_step (ps1|>X.x_ps1) fs)

let dest_f_finished: ('k,'v,'r) find_state -> 
      ('r * 'k * 'r * ('k * 'v) list * ('k,'r) rstk) option = (
  fun fs -> Find.dest_f_finished fs |> (function
      | None -> None
      | Some  x -> Some (x5 x)))
  
let wellformed_find_state: 
  'k ps0 -> ('k,'v,'r) r2f -> ('k,'v) tree -> ('k,'v,'r) find_state -> bool = 
  (fun ps0 r2f t fs -> 
     Find.wellformed_find_state (ps0.compare_k|>X.x_cmp) r2f t fs)


(* delete ---------------------------------------- *)
let mk_delete_state: 'k -> 'r -> ('k,'v,'r) delete_state = Delete.mk_delete_state

let delete_step: ('k,'v,'r,'t) ps1 -> ('k,'v,'r) delete_state -> 't -> 
      't * ('k,'v,'r) delete_state res = 
  (fun ps1 ds -> Delete.delete_step (ps1|>X.x_ps1) ds)

let dest_d_finished: ('k,'v,'r) delete_state -> 'r option = Delete.dest_d_finished

let wellformed_delete_state: 'k ps0 -> ('k,'v,'r) r2f -> ('k,'v) tree -> 'k -> 
      ('k,'v,'r) delete_state -> bool = (
  fun ps0 r2f t k ds -> 
    Delete.wellformed_delete_state (ps0|>X.x_ps0) r2f t k ds)
    

(* insert ---------------------------------------- *)
let mk_insert_state: 'k -> 'v -> 'r -> ('k,'v,'r) insert_state = Insert.mk_insert_state

let insert_step: ('k,'v,'r,'t) ps1 -> ('k,'v,'r) insert_state -> 't -> 
      't * ('k,'v,'r) insert_state res = 
  (fun ps1 is -> Insert.insert_step (ps1|>X.x_ps1) is)

let dest_i_finished: ('k,'v,'r)insert_state -> 'r option = Insert.dest_i_finished

let wellformed_insert_state: 'k ps0 -> ('k,'v,'r) r2f -> ('k,'v) tree -> 'k -> 'v -> 
      ('k,'v,'r) insert_state -> bool = 
  (fun ps0 r2f t k v is ->
     Insert.wellformed_insert_state (ps0|>X.x_ps0) r2f t k v is)

(* insert_many ---------------------------------------- *)
let mk_im_state: 'k -> 'v -> ('k*'v) list -> 'r -> ('k,'v,'r) im_state = 
  Insert_many.mk_insert_state

let im_step: ('k,'v,'r,'t) ps1 -> ('k,'v,'r) im_state -> 't -> 
      't * ('k,'v,'r) im_state res = 
  (fun ps1 is -> Insert_many.insert_step (ps1|>X.x_ps1) is)

let dest_im_finished: ('k,'v,'r)im_state -> ('r*('k*'v)list) option = 
  Insert_many.dest_i_finished
  (* no wf *)


(* leaf stream ---------------------------------------- *)

let mk_ls_state : 'r -> ('k,'v,'r) ls_state = Leaf_stream.mk_ls_state

let ls_step ps1 ls: 't -> 't * ('k,'v,'r) ls_state res = 
  Leaf_stream.lss_step (ps1|>X.x_ps1) ls

let ls_dest_leaf ls = Leaf_stream.dest_LS_leaf ls

let ls_is_finished lss : bool = (
  lss |> Leaf_stream.lss_is_finished)

