(** Single entry point for executable examples *)

let args = Sys.argv |> Array.to_list |> List.tl
  
let fn = "btree.store" (* FIXME config *)

let usage = Printf.sprintf {|

Usage: 
  <xxx.exe>          (to print usage)
  <xxx.exe> example  (to run the example)             
  <xxx.exe> args     (to run the "main" executable)

Documentation for main:

%s
|}
              Generic_main.usage

let _ : unit = 
  match args with
  | [] -> (print_endline usage; exit 0)
(*  | ["example"] -> 
    let prg = Generic_example.make_1 () in    
    Lwt_main.run (With_lwt.to_lwt prg) *)
  | args -> 
    let prg = Generic_main.Int_int.main args in
    Lwt_main.run (With_lwt.to_lwt prg)

