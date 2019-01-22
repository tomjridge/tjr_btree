(** Control isabelle inessential checks via flag *)
open Isa_btree

let _ = 
  Tjr_fs_shared.Global.register 
    ~name:"Isa_export.check_flag" 
    Isa_btree.Isa_export.check_flag

let enable_isa_checks () = Isa_export.check_flag:=true
let disable_isa_checks () = Isa_export.check_flag:=false

