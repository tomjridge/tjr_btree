(** Single entry point for executable examples *)

let args = Sys.argv |> Array.to_list |> List.tl
  
let fn = "btree.store" (* FIXME config *)

let _ = 
  match args with
  | [] -> (print_endline "usage"; exit 0)
  | ["1"] -> 
    let prg = Generic_example.make_1 () in    
    Lwt_main.run (With_lwt.to_lwt prg)

