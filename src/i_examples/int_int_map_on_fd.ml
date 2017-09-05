(* a map from int to int, backed by file ------------------------------- *)

open Map_ops
open Map_int_int


let main args = (
  (* turn off wf checking *)
  Test.disable ();
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  match args with
  | ["init"; fn] -> (
      from_file ~fn ~create:true ~init:true
      |> (fun _ -> print_endline "init ok"))
  | ["insert";fn;k;v] -> (
      from_file ~fn ~create:false ~init:false
      |> insert (int_of_string k) (int_of_string v) 
      |> function (t,Ok _) -> (close t))
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false
      |> delete (int_of_string k) 
      |> function (t,Ok _) -> (close t))
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false
      |> (fun s -> 
          s 
          |> Leaf_stream_ops.all_kvs ~ls_ops 
          |> (function (s',Ok kvs) -> (
                (List.iter (fun (k,v) -> 
                     Printf.printf "%s -> %s\n" (string_of_int k) 
                       (string_of_int v)) kvs);
                close s';
                ()));                
          print_endline "list ok"))
  | _ -> (
      failwith (
        "Unrecognized args: "^
        (String_.concat_strings " " args)^ 
        __LOC__)) )
