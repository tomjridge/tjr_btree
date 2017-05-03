(* prelude ---------------------------------------- *)

(* collect together modules, then open Prelude at top of following *)

(* module Btree_util = Btree_util *)
(* module Functional_store = Functional_store *)
(* module World = World_monad.World *)

(* some renaming *)
(*
module Mut = State_error_monad.Mut
module Sem = State_error_monad.Sem
module State_error_monad = State_error_monad.State_error_monad
*)

(*
module type MONAD = sig
  type 'a m
  val bind: ('a -> 'b m) -> 'a m -> 'b m
  val return: 'a -> 'a m
end
*)

open Base_types

module Small_step = Bt_small_step

include Prelude_pervasives

include Pickle_params

type 'k ord = 'k -> 'k -> int

(* so we can just open Prelude *)
include Small_step.O

include Test


module Params = struct
  include Base_types_params
      
  let pp x : ('k,'v) pp = (x#pp)
end

include Params


(* r2t ---------------------------------------- *)

(* FIXME not sure where these should go *)

let mk_r2f store_ops : ('k,'v,'r,'t) r2f = (
  fun s r ->
    s |> store_ops.store_read r 
    |> function (s',Ok f) -> Some f | _ -> (ignore(failwith __LOC__); None))

let mk_r2t r2f = Isa_export.Pre_params.mk_r2t r2f (Small_step.X.int_to_nat 1000)

let store_ops_to_r2t store_ops = mk_r2t (mk_r2f store_ops)
