module IE = Isa_export
open IE
open Isa_util_params
open Iu_pervasives

module O = struct
  (* safe to open O in other modules *)
  include Store_ops

  type ('k,'r) rstk = ('k,'r,unit) Tree_stack.ts_frame_ext list
  type ('k,'v,'r,'t) r2f = ('t -> 'r -> ('k,'v,'r) frame option) 

  type ('k,'v,'r) find_state = ('k,'v,'r) IE.Find.find_state
  type ('k,'v,'r) insert_state = ('k,'v,'r) IE.Insert.insert_state
  type ('k,'v,'r) im_state = ('k,'v,'r) IE.Insert_many.ist
  type ('k,'v,'r) delete_state = ('k,'v,'r) IE.Delete.delete_state
  type ('k,'v,'r) ls_state = ('k,'v,'r) IE.Leaf_stream.ls_state
end

include O

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
  let x_cmp cmp x y = cmp x y |> int_to_int
  let x_ps0 ps : 'k IE.Params.ps0 = (
    Ps0(
      (constants ps)|>x_constants,
      (compare_k ps)|>x_cmp
    ))

  let x_store_ops store_ops : ('k,'v,'r,'t,unit) IE.Params.store_ops_ext = (
    Store_ops_ext(
      store_ops.store_read,
      store_ops.store_alloc,
      store_ops.store_free,()))

  let x_ps1 ps : ('k,'v,'r,'t) IE.Params.ps1 = IE.Params.(
      Ps1(ps|>x_ps0, (store_ops ps)|>x_store_ops))

end

let x5 (x,(y,(z,(w,u)))) = (x,y,z,w,u)

(* find ---------------------------------------- *)

let  mk_find_state k r : ('k,'v,'r) find_state = Find.mk_find_state k r

let find_step ps : 'fs->('fs,'t) m = (fun fs -> Find.find_step (ps|>X.x_ps1) fs)

let dest_f_finished fs: ('r * 'k * 'r * 'kvs * ('k,'r) rstk) option = (
    Find.dest_f_finished fs 
    |> (function None -> None | Some  x -> Some (x5 x)))

(* only check if we have access to the relevant r2t and spec_tree *)
(* ASSUMES r2t ps <> None *)
let wellformed_find_state ps: 'tree -> 't -> ('k,'v,'r) find_state -> bool = (
  fun t s fs -> 
    Find.wellformed_find_state 
      (compare_k ps|>X.x_cmp) (debug ps|>dest_Some).r2t t s fs)
  

(* delete ---------------------------------------- *)

let mk_delete_state: 'k -> 'r -> ('k,'v,'r) delete_state = Delete.mk_delete_state

let delete_step ps: 'ds -> ('ds,'t) m = 
  (fun ds -> Delete.delete_step (ps|>X.x_ps1) ds)

let dest_d_finished: ('k,'v,'r) delete_state->'r option = Delete.dest_d_finished

let wellformed_delete_state ps: 'tree->'t->'k->('k,'v,'r) delete_state->bool = (
  fun t s k ds -> 
    Delete.wellformed_delete_state 
      (ps|>X.x_ps0) (debug ps|>dest_Some).r2t t s k ds)
  

(* insert ---------------------------------------- *)

let mk_insert_state: 'k->'v->'r->('k,'v,'r) insert_state = Insert.mk_insert_state

let insert_step ps: 'is -> ('is,'t) m = 
  (fun is -> Insert.insert_step (ps|>X.x_ps1) is)

let dest_i_finished: ('k,'v,'r)insert_state -> 'r option = Insert.dest_i_finished

let wellformed_insert_state ps:'tree->'t->'k->'v->('k,'v,'r) insert_state->bool = (
  fun t s k v is ->
    Insert.wellformed_insert_state 
      (ps|>X.x_ps0) (debug ps|>dest_Some).r2t t s k v is)

(* insert_many ---------------------------------------- *)

let mk_im_state: 'k -> 'v -> ('k*'v) list -> 'r -> ('k,'v,'r) im_state = 
  Insert_many.mk_insert_state

let im_step ps: 'im -> ('im,'t) m = 
  (fun is -> Insert_many.insert_step (ps|>X.x_ps1) is)

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


