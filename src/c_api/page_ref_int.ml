(* fix page_ref --------------------------------------------------- *)

(** Utility module to fix page_ref as int *)
type page_ref = int  [@@deriving yojson]
type ('k,'v) frame = ('k,'v,page_ref) Frame.frame


