(** Prelude, before the main action. *)

(* collect together modules, then open Prelude at top of following *)

(* FIXME not sure about the Prelude and Base_types and opening and including *)
include Base_types_pervasives

open Base_types

(* so we can just open Prelude *)
include Small_step.O

include Test


