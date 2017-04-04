module X = struct

  let int_to_nat x = Isa_export.(x |>Big_int.big_int_of_int|>Arith.nat_of_integer)
  let int_to_int x = Isa_export.(
      x |>Big_int.big_int_of_int|>(fun x -> Arith.Int_of_integer x))

end

module IE = Isa_export

module Frame = IE.Frame

type constants = {
  min_leaf_size: int;
  max_leaf_size: int;
  min_node_keys: int;
  max_node_keys: int
}

module type PARAMS = sig  
  type k
  type v
  val compare_k: k -> k -> int
  val equal_v: v -> v -> bool
  type store
  type 'a m = M of (store -> store * ('a,string) result)
  type page_ref
  val cs0 : constants
  val store_free : page_ref list -> unit m
  val store_read : page_ref -> (k, v, page_ref) Frame.t m
  val store_alloc : (k, v, page_ref) Frame.t -> page_ref m
end

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
      let m2mm = function P.M(f) -> MM(fun x -> x |> f |> (fun (s',r) -> (s',
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
    include Isa_export.Make(S_)
end)

