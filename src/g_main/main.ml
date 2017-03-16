(* main entry point *)

let args = Sys.argv |> Array.to_list |> List.tl

let _ = 
  match args with
  | "kv"::rest -> (Kv_store_small.main rest)
  | _ -> failwith ("Unrecognized arguments: "^
                   (Tjr_string.concat_strings " " args)^
                   __LOC__)
