(** Main entry point. At the moment this is only used by [kv_main.sh] *)

let args = Sys.argv |> Array.to_list |> List.tl

let _ = 
  match args with
  | "kv"::rest -> (Ss_ss_map_on_fd.main rest)
  | _ -> failwith ("Unrecognized arguments: "^
                   (String_.concat_strings " " args)^
                   __LOC__)
