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
  (* TODO rename store to world? *)
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

(* wrapper around isa_export.make; aim to use this rather than
   isa_export.make *)
module Make = functor (P:PARAMS) -> (struct
    module S_ = struct
      let dummy=()
      type k = P.k
      type v = P.v
      let equal_k = IE.HOL.{ equal=(fun k1 k2 -> P.compare_k k1 k2 = 0) }
      let equal_v = IE.HOL.{ equal=P.equal_v }
      type store = P.store
      type 'a mm = MM of (store -> store * 'a IE.Util.res)
      type page_ref = P.page_ref
      let constants: unit IE.Prelude.constants_ext = IE.Prelude.Constants_ext(
          P.cs0.min_leaf_size|>X.int_to_nat,
          P.cs0.max_leaf_size|>X.int_to_nat,
          P.cs0.min_node_keys|>X.int_to_nat,
          P.cs0.max_node_keys|>X.int_to_nat,
          ())
      let compare_k: (k, unit) IE.Key_value.key_order_ext = IE.Key_value.(
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
    module Tree = struct
      type ('k,'v) tree = ('k,'v) IE.Tree.tree
    end
    type 'a m = P.store -> P.store * ('a,string)result
    let dest_MM = function S_.MM f -> (fun s -> 
        f s 
        |> (fun (s',r) -> (s',
                           match r with 
                           | IE.Util.Ok x -> Ok x
                           | IE.Util.Error (String_error y) -> Error y)))
    let mm2m = dest_MM
    let lift f x = x |> f |>dest_MM

    module Find = struct
      type find_state = IEM.Find.find_state
      let mk_find_state = IEM.Find.mk_find_state
      let find_step: find_state -> find_state m = lift IEM.Find.find_step
      (* modify dest_f_finished to omit some unneeded info? FIXME remove? *)
      let dest_f_finished fs = fs |> IEM.Find.dest_f_finished |> (fun r ->
          match r with
          | None -> None
          | Some (r0, (k, (r, (kvs, stk)))) -> Some(r,kvs))
      let wellformed_find_state = IEM.Find.wellformed_find_state
    end

    module Insert = struct
      type insert_state = IEM.Insert.insert_state
      let mk_insert_state = IEM.Insert.mk_insert_state
      let insert_step: insert_state -> insert_state m = 
        lift IEM.Insert.insert_step
      let dest_i_finished = IEM.Insert.dest_i_finished
      let wellformed_insert_state = IEM.Insert.wellformed_insert_state
    end                             

    module Insert_many = struct
      type i_state_t = IEM.Insert_many.i_state_t 
      let mk_insert_state = IEM.Insert_many.mk_insert_state
      let insert_many_step: i_state_t -> i_state_t m = 
        lift IEM.Insert_many.insert_step
      let dest_im_finished = IEM.Insert_many.dest_i_finished (* NB rename *)
    end                             

    module Delete = struct
      type delete_state = IEM.Delete.delete_state
      let mk_delete_state = IEM.Delete.mk_delete_state
      let delete_step: delete_state -> delete_state m = 
        lift IEM.Delete.delete_step
      let dest_d_finished = IEM.Delete.dest_d_finished
      let wellformed_delete_state = IEM.Delete.wellformed_delete_state
    end

    module Leaf_stream = struct 
      type ls_state = (P.k,P.v,P.page_ref) Isa_export.Pre_params.ls_state
      let mk_ls_state : P.page_ref -> ls_state = IEM.Leaf_stream.mk_ls_state
      let lss_step: ls_state -> ls_state m = lift IEM.Leaf_stream.lss_step
      let dest_LS_leaf: ls_state -> (P.k*P.v) list option = 
        IEM.Leaf_stream.dest_LS_leaf
      let lss_is_finished : ls_state -> bool = IEM.Leaf_stream.lss_is_finished
    end

end)
