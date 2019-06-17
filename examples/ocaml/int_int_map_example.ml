(** A simple example of a kv store. *)

(* open Examples *)
open Generic_example

let int_to_k = fun x -> x
let int_to_v = fun x -> x

let on_disk_util = Examples.On_disk.Int_int.on_disk_util
let root_ops_to_map_ops_etc root_ops = Examples.On_disk.Int_int.map_ops_etc ~root_ops

let make_generic_example () = 
  make_generic_example ~on_disk_util ~root_ops_to_map_ops_etc 
    ~int_to_k ~int_to_v

let do_all () = 
  make_generic_example () @@ fun ~do_all ~btree_from_file:_ ~map_ops_etc:_ -> 
  do_all()
