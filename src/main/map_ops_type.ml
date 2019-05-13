(** Map operations *)

(* open Tjr_monad.Types *)

type ('k,'v,'t) map_ops = {
  find: k:'k -> ('v option, 't)m;
  insert: k:'k -> v:'v -> (unit,'t) m;
  delete: k:'k -> (unit,'t)m;
}
