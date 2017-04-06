module X = struct

  let int_to_nat x = Isa_export.(x |>Big_int.big_int_of_int|>Arith.nat_of_integer)
  let int_to_int x = Isa_export.(
      x |>Big_int.big_int_of_int|>(fun x -> Arith.Int_of_integer x))

end

module IE = Isa_export

module Frame = IE.Frame

type constants = Constants.t

type 'a res = ('a,string) result

module type PARAMS = sig  
  type k
  type v
  val compare_k: k -> k -> int
  val equal_v: v -> v -> bool
  type store
  type 'a m = (store -> store * 'a res)
  type page_ref
  val cs0 : constants
  (* for wf checking ; this requires that store is the state rather
     than a world ref *)
  val mk_r2f : store -> page_ref -> (k, v, page_ref) Frame.t option
  val store_free : page_ref list -> unit m
  val store_read : page_ref -> (k, v, page_ref) Frame.t m
  val store_alloc : (k, v, page_ref) Frame.t -> page_ref m
end

(*
module Poly_params = struct
  type page_ref = int
  type ('a,'s) m = ('s -> 's * 'a res)
  type ('k,'v,'s) t = {
    compare_k: 'k -> 'k -> int;
    equal_v: 'v -> 'v -> bool;
    cs0: constants;
    store_free: page_ref list -> (unit,'s) m;
    store_read : page_ref -> (('k, 'v,page_ref) Frame.t,'s) m;
    store_alloc : ('k, 'v,page_ref) Frame.t -> (page_ref,'s) m;
    mk_r2f: 's -> page_ref -> ('k,'v,page_ref) Frame.t option;
(* FIXME    t: 's World.r     *)
  }
end
*)

(* wrapper around isa_export.make *)
module Make = functor (P:PARAMS) -> (struct
    module P = P
    module S_ = struct
      type k = P.k
      type v = P.v
      let equal_k = IE.HOL.{ equal=(fun k1 k2 -> P.compare_k k1 k2 = 0) }
      let equal_v = IE.HOL.{ equal=P.equal_v }
      type store = P.store
      type 'a mm = MM of (store -> store * 'a IE.Util.res)
      type page_ref = P.page_ref
      let cs0: unit IE.Prelude.constants_ext = IE.Prelude.Constants_ext(
          P.cs0.min_leaf_size|>X.int_to_nat,
          P.cs0.max_leaf_size|>X.int_to_nat,
          P.cs0.min_node_keys|>X.int_to_nat,
          P.cs0.max_node_keys|>X.int_to_nat,
          ())
      let ord0: (k, unit) IE.Key_value.key_order_ext = IE.Key_value.(
          Key_order_ext ((fun k1 k2 -> P.compare_k k1 k2 < 0), ())
      )
      let mk_r2f: store -> page_ref -> (k, v, page_ref) Frame.t option = (
        P.mk_r2f)
      let m2mm = function f -> MM(fun x -> x |> f |> (fun (s',r) -> (s',
          match r with
            Ok x -> IE.Util.Ok x
          | Error y -> IE.Util.Error (IE.Util.String_error y))))

      let store_free : page_ref list -> unit mm = (
        fun rs -> P.store_free rs |> m2mm)
      let store_read : page_ref -> (k, v, page_ref) Frame.t mm = (
        fun r -> P.store_read r |> m2mm)
      let store_alloc : (k, v, page_ref) Frame.t -> page_ref mm = (
        fun f -> P.store_alloc f |> m2mm)
    end
    module IEM = Isa_export.Make(S_)
    type 'a m = P.store -> P.store * ('a,string)result
    let dest_MM = function S_.MM f -> (fun s -> 
        f s 
        |> (fun (s',r) -> (s',
                           match r with 
                           | IE.Util.Ok x -> Ok x
                           | IE.Util.Error (String_error y) -> Error y)))
    let mm2m = dest_MM
    module Find_ = IEM.Find                    
    open Find_
    let lift f x = x |> f |>dest_MM
    let find_step: find_state -> find_state m = lift find_step
    let dest_f_finished fs = fs |> Find_.dest_f_finished |> (fun r ->
        match r with
        | None -> None
        | Some (r0, (k, (r, (kvs, stk)))) -> Some(r,kvs))

    module Insert_ = IEM.Insert
    open Insert_
    let insert_step: insert_state -> insert_state m = lift insert_step
    let dest_i_finished is = is |> Insert_.dest_i_finished
                                     
    module Insert_many_ = IEM.Insert_many
    open Insert_many_
    let insert_many_step: i_state_t -> i_state_t m = lift insert_step
    let dest_im_finished is = is |> Insert_many_.dest_i_finished


    module Delete_ = IEM.Delete
    open Delete_
    let delete_step: delete_state -> delete_state m = lift delete_step
    let dest_d_finished ds = ds |> Delete_.dest_d_finished 
    (* FIXME further wrappers go here *)

end)

(* Isa_export.Make signatures:

module Find : sig
  type btree
  type find_state
  val mk_r2t :
    (Params.page_ref -> (Params.k, Params.v, Params.page_ref) Frame.t option) ->
      Arith.nat -> Params.page_ref -> (Params.k, Params.v) Tree.tree option
  val find_step : find_state -> find_state Params.mm
  val empty_btree : unit -> btree Params.mm
  val mk_find_state : Params.k -> Params.page_ref -> find_state
  val wf_store_tree :
    Params.store -> Params.page_ref -> (Params.k, Params.v) Tree.tree -> bool
  val dest_f_finished :
    find_state ->
      (Params.page_ref *
        (Params.k *
          (Params.page_ref *
            ((Params.k * Params.v) list *
              (Params.k, Params.page_ref, unit)
                Tree_stack.ts_frame_ext list)))) option
  val wellformed_find_state :
    Params.store -> (Params.k, Params.v) Tree.tree -> find_state -> bool
end = struct

module Insert : sig
  type i12_t = I1 of Params.page_ref |
    I2 of (Params.page_ref * (Params.k * Params.page_ref))
  type insert_state = I_down of (Find.find_state * Params.v) |
    I_up of
      (i12_t * (Params.k, Params.page_ref, unit) Tree_stack.ts_frame_ext list)
    | I_finished of Params.page_ref
  val insert_step : insert_state -> insert_state Params.mm
  val dest_i_finished : insert_state -> Params.page_ref option
  val mk_insert_state : Params.k -> Params.v -> Params.page_ref -> insert_state
  val wellformed_insert_state :
    Params.store ->
      (Params.k, Params.v) Tree.tree ->
        Params.k -> Params.v -> insert_state -> bool
end = struct

module Delete : sig
  type del_t = D_small_leaf of (Params.k * Params.v) list |
    D_small_node of (Params.k list * Params.page_ref list) |
    D_updated_subtree of Params.page_ref
  type delete_state = D_down of (Find.find_state * Params.page_ref) |
    D_up of
      (del_t *
        ((Params.k, Params.page_ref, unit) Tree_stack.ts_frame_ext list *
          Params.page_ref))
    | D_finished of Params.page_ref
  val delete_step : delete_state -> delete_state Params.mm
  val dest_d_finished : delete_state -> Params.page_ref option
  val mk_delete_state : Params.k -> Params.page_ref -> delete_state
  val wellformed_delete_state :
    (Params.k, Params.v) Tree.tree ->
      Params.k -> Params.store -> delete_state -> bool
end = struct

module Insert_many : sig
  type i_t = I1 of (Params.page_ref * (Params.k * Params.v) list) |
    I2 of ((Params.page_ref * (Params.k * Params.page_ref)) *
            (Params.k * Params.v) list)
  type i_state_t =
    I_down of (Find.find_state * (Params.v * (Params.k * Params.v) list)) |
    I_up of
      (i_t * (Params.k, Params.page_ref, unit) Tree_stack.ts_frame_ext list)
    | I_finished of (Params.page_ref * (Params.k * Params.v) list)
  val insert_step : i_state_t -> i_state_t Params.mm
  val dest_i_finished :
    i_state_t -> (Params.page_ref * (Params.k * Params.v) list) option
  val mk_insert_state :
    Params.k ->
      Params.v -> (Params.k * Params.v) list -> Params.page_ref -> i_state_t
end = struct

module Leaf_stream : sig
  type ls_state
  val lss_step : ls_state -> ls_state Params.mm
  val mk_ls_state : Params.page_ref -> ls_state
  val dest_LS_leaf : ls_state -> ((Params.k * Params.v) list) option
  val lss_is_finished : ls_state -> bool
end = struct


*)
