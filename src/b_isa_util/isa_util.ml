module IE = Isa_export
open IE

module O = struct
  (* safe to open O in other modules *)
  include Store_ops
  include Isa_util_params

  type ('k,'v,'r) frame = ('k,'v,'r) Frame.frame
  type ('k,'v) tree = ('k,'v) Tree.tree
  type ('k,'r) rstk = ('k,'r,unit) Tree_stack.ts_frame_ext list

  type ('k,'v,'r,'t) r2f = ('t -> 'r -> ('k,'v,'r) frame option) 
  type ('k,'v,'r,'t) r2t = ('t -> 'r -> ('k,'v) tree option) 

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
  let x_ps0 ps0 : 'k IE.Params.ps0 = (
    Ps0(
      ps0.constants|>x_constants,
      x_cmp ps0.compare_k
    ))

  let x_store_ops store_ops : ('k,'v,'r,'t,unit) IE.Params.store_ops_ext = (
    Store_ops_ext(
      store_ops.store_read,
      store_ops.store_alloc,
      store_ops.store_free,()))

  let x_ps1 ps1 : ('k,'v,'r,'t) IE.Params.ps1 = IE.Params.(
      Ps1(ps1.ps0|>x_ps0, ps1.store_ops|>x_store_ops))
  
end

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
  'k ps0 -> ('k,'v,'r,'t) r2t -> ('k,'v) tree -> 't -> ('k,'v,'r) find_state -> bool = 
  (fun ps0 r2t t s fs -> 
     Find.wellformed_find_state (ps0.compare_k|>X.x_cmp) r2t t s fs)


(* delete ---------------------------------------- *)
let mk_delete_state: 'k -> 'r -> ('k,'v,'r) delete_state = Delete.mk_delete_state

let delete_step: ('k,'v,'r,'t) ps1 -> ('k,'v,'r) delete_state -> 't -> 
      't * ('k,'v,'r) delete_state res = 
  (fun ps1 ds -> Delete.delete_step (ps1|>X.x_ps1) ds)

let dest_d_finished: ('k,'v,'r) delete_state -> 'r option = Delete.dest_d_finished

let wellformed_delete_state: 
  'k ps0 -> ('k,'v,'r,'t) r2t -> ('k,'v) tree -> 't -> 'k -> 
  ('k,'v,'r) delete_state -> bool = (
  fun ps0 r2t t s k ds -> 
    Delete.wellformed_delete_state (ps0|>X.x_ps0) r2t t s k ds)
    

(* insert ---------------------------------------- *)
let mk_insert_state: 'k -> 'v -> 'r -> ('k,'v,'r) insert_state = Insert.mk_insert_state

let insert_step: ('k,'v,'r,'t) ps1 -> ('k,'v,'r) insert_state -> 't -> 
      't * ('k,'v,'r) insert_state res = 
  (fun ps1 is -> Insert.insert_step (ps1|>X.x_ps1) is)

let dest_i_finished: ('k,'v,'r)insert_state -> 'r option = Insert.dest_i_finished

let wellformed_insert_state: 'k ps0 -> ('k,'v,'r,'t) r2t -> ('k,'v) tree 
  -> 't -> 'k -> 'v -> ('k,'v,'r) insert_state -> bool = 
  (fun ps0 r2t t s k v is ->
     Insert.wellformed_insert_state (ps0|>X.x_ps0) r2t t s k v is)

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


(* r2t ---------------------------------------- *)

let mk_r2f store_ops : ('k,'v,'r,'t) r2f = (
  fun s r ->
    s |> store_ops.store_read r 
    |> function (s',Ok f) -> Some f | _ -> (ignore(failwith __LOC__); None))

let mk_r2t r2f = Isa_export.Pre_params.mk_r2t r2f (X.int_to_nat 1000)

let store_ops_to_r2t store_ops = mk_r2t (mk_r2f store_ops)
