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

include Pervasives_

type 'k ord = 'k -> 'k -> int

(* so we can just open Prelude *)
include Isa_util.O

include Test
