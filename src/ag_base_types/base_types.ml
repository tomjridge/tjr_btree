(** Collect base types together with sensible names *)

include Base_types_pervasives
include Tree
include Frame
include R2t
include Tjr_monad.Monad


(** Monadic reference operations *)
type ('a,'s) mref = {
  get: unit -> ('a,'s) m;
  set: 'a -> (unit,'s) m
}
