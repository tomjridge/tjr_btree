(** Print out names of registered globals *)

(* make sure some modules are linked in *)
let _ = Tjr_btree.Test_exhaustive_in_mem.main

let _ = 
  Tjr_fs_shared.Global.Internal.print_all_names()
  
