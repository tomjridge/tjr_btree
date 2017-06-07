(** Control isabelle inessential checks via flag *)
let enable_isa_checks () = Isa_export.check_flag:=true
let disable_isa_checks () = Isa_export.check_flag:=false

