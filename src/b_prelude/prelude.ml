(* prelude ---------------------------------------- *)

(* collect together modules, then open Prelude at top of following *)

(* module Btree_util = Btree_util *)
(* module Functional_store = Functional_store *)
(* module World = World_monad.World *)

(* some renaming *)
module Mut = State_error_monad.Mut
module Sem = State_error_monad.Sem
module State_error_monad = State_error_monad.State_error_monad

module type MONAD = sig
  type 'a m
  val bind: ('a -> 'b m) -> 'a m -> 'b m
  val return: 'a -> 'a m
end

module Util = struct
  let flush_out () = flush Pervasives.stdout
  let read_file fn = (BatPervasives.input_file fn)
  let rec iter_step (f:'s -> 's option) (x:'s) = (
    let s' = f x in
    match s' with
    | None -> x
    | Some x' -> iter_step f x')
end

include Pervasives_
