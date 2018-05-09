open Tree

(** The r2t type represents a pure view of a given system state. It is
   constructed as a function from system state. The result is a
   function which takes a reference r and constructs the (key,value)
   tree rooted at r. In case the state is not wellformed, the return
   type is actually a tree option. NOTE this function takes the global
   state t, not some particular store state. *)
type ('k,'v,'r,'t) r2t = ('t -> 'r -> ('k,'v) tree option)



(* r2t ---------------------------------------- *)

(* FIXME not sure where these should go; r2t.ml? *)

let int_to_nat x = Isa_export.(x |>Big_int.big_int_of_int|>Arith.nat_of_integer)

let mk_r2t r2f = Isa_export.Pre_params.mk_r2t r2f (int_to_nat 1000)


(* let store_ops_to_r2t store_read = mk_r2t (mk_r2f ~store_read) *)

let store_read_to_r2t ~run ~store_read = mk_r2t (R2f.mk_r2f ~run ~store_read)

let _ = store_read_to_r2t
