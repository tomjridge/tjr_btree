open Tjr_btree
open Small_string
open Ss_ss_map_on_fd

let run = Tjr_step_monad.Extra.run

let main args = 
  (* turn off wf checking *)
  Isa_test.disable_isa_checks();
  Test.disable ();
  Map_ops.dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  match args with
  | ["init"; fn] -> (
      from_file ~fn ~create:true ~init:true |> fun _ -> 
      print_endline "init ok")
  | ["insert";fn;k;v] -> (
      from_file ~fn ~create:false ~init:false |> fun s -> 
      insert (SS.of_string k) (SS.of_string v) |> run s |> fun (t, _) -> 
      close t)
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false |> fun s ->
      delete (SS.of_string k) 
      |> run s
      |> function (t,_) -> close t)
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false |> fun s -> 
      Leaf_stream_util.all_kvs ~ls_ops 
      |> run s 
      |> (function (s',kvs) -> (
            (List.iter (fun (k,v) -> 
                 Printf.printf "%s -> %s\n" (SS.to_string k) 
                   (SS.to_string v)) kvs);
            close s';
            ()));                
      print_endline "list ok")
  | _ -> (
      failwith @@ (
        "Unrecognized args: "^
        String_.concat_strings " " args^
        __LOC__))

let _ = main (Sys.argv |> Array.to_list |> List.tl)
