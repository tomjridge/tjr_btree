(** Prelude, before the main action. *)

(* collect together modules, then open Prelude at top of following *)

(* FIXME not sure about the Prelude and Base_types and opening and including *)
include Base_types_pervasives

open Base_types

(* FIXME in base types? remove? *)
type 'k ord = 'k -> 'k -> int

(* so we can just open Prelude *)
include Small_step.O

include Test


(* r2t ---------------------------------------- *)

(* FIXME not sure where these should go; r2t.ml? *)

let mk_r2f ~store_read : ('k,'v,'r,'t) r2f = (
  fun s r ->
    s |> store_read r 
    |> function (s',Ok f) -> Some f | _ -> (ignore(failwith __LOC__); None))

let mk_r2t r2f = Isa_export.Pre_params.mk_r2t r2f (Small_step.X.int_to_nat 1000)


(* let store_ops_to_r2t store_read = mk_r2t (mk_r2f ~store_read) *)


let store_read_to_r2t store_read = mk_r2t (mk_r2f ~store_read)
