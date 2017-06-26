(* fix page_ref --------------------------------------------------- *)

open Bin_prot.Std

(** Utility module to fix page_ref as int *)
type page_ref = int [@@deriving bin_io, yojson] 
type ('k,'v) frame = ('k,'v,page_ref) Frame.frame


