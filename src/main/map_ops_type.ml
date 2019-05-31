(** Map operations *)

(* open Tjr_monad.Types *)

type ('k,'v,'t) map_ops = {
  find: k:'k -> ('v option, 't)m;
  insert: k:'k -> v:'v -> (unit,'t) m;
  delete: k:'k -> (unit,'t)m;
}


(*
type ('k,'v,'r,'leaf,'ls_impl,'t) extra_map_ops = {
  insert_many:
  insert_all: kvs:('k*'v)list -> (unit, 't)m;
  leaf_stream_ops: ('k,'v,'r,'ls_impl,'t) leaf_stream_ops;
}
*)

type ('a,'b,'c) extra_map_ops = {
  insert_many: 'a;
  insert_all: 'b;
  leaf_stream_ops: 'c;
}
