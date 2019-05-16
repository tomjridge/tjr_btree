(** Single entry point for executable examples *)
(* open Int_int_map_example_functionality *)

let args = Sys.argv |> Array.to_list |> List.tl

(*
let _ = 
  do_mini_check(); 
  do_full_check() 
*)

let _ = Int_int_map_main.main args
