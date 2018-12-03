(* a map from int to int, backed by file ------------------------------- *)

open Tjr_btree
open Map_int_int

let run = Tjr_monad.State_passing.run

let monad_ops = Tjr_monad.State_passing.monad_ops ()

let main args =
  (* turn off wf checking *)
  Isa_test.disable_isa_checks();
  Test.disable ();
  Map_ops.dest_map_ops map_ops @@ fun ~find:_ ~insert ~delete ~insert_many:_ ->
  match args with
  | ["init"; fn] -> (
      from_file ~fn ~create:true ~init:true |> fun _ -> 
      print_endline "init ok")
  | ["insert";fn;k;v] -> (
      from_file ~fn ~create:false ~init:false |> fun s ->
      insert (int_of_string k) (int_of_string v)  
      |> run ~init_state:s 
      |> function (_,t) -> close t)
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false |> fun s ->
      delete (int_of_string k) 
      |> run ~init_state:s 
      |> function (_,t) -> close t)
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false |> fun s -> 
      Leaf_stream_util.all_kvs ~monad_ops ~ls_ops ~r:s.Map_on_fd.Default_implementation.root
      |> run ~init_state:s 
      |> (function (kvs,s') -> (
            (List.iter (fun (k,v) -> 
                 Printf.printf "%s -> %s\n" (string_of_int k) 
                   (string_of_int v)) kvs);
            close s';
            ()));                
      print_endline "list ok")
  | ["nop"] -> (
      (* print_endline "nop ok" *)
    )
  | _ -> (
      failwith (
        "Unrecognized args: "^
        (String_.concat_strings ~sep:" " args)^ 
        __LOC__))

let _ = main (Sys.argv |> Array.to_list |> List.tl)

