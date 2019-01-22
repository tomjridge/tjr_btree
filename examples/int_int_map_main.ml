(* a map from int to int, backed by file ------------------------------- *)

open Tjr_btree
open Examples

let (from_file,close,rest) = Examples.ii_map_on_fd

let main args =
  (* turn off wf checking *)
  Isa_test.disable_isa_checks();
  Test.disable ();
  match args with
  | ["init"; fn] ->
    let state = from_file ~fn ~create:true ~init:true in
    print_endline "init ok";
    close state    

  | ["insert";fn;k;v] -> (
      let state = from_file ~fn ~create:true ~init:true in
      let (_,insert,_) = rest ~state in
      insert (int_of_string k) (int_of_string v);
      close state)

  | ["delete";fn;k] -> (
      let state = from_file ~fn ~create:true ~init:true in
      let (_,_,delete) = rest ~state in
      delete (int_of_string k);
      close state)

  | ["list";fn] -> (
      let monad_ops = Map_on_fd_util.monad_ops in
      let state = from_file ~fn ~create:true ~init:true in
      let r = state.root in
      Leaf_stream_util.all_kvs ~monad_ops ~ls_ops ~r
      |> run ~init_state:s 
      |> (function (kvs,s') -> (
            (List.iter (fun (k,v) -> 
                 Printf.printf "%s -> %s\n" (string_of_int k) 
                   (string_of_int v)) kvs);
            close s';
            ()));                
      print_endline "list ok")

  | ["insert_range";fn;l;h] -> (
      from_file ~fn  ~create:false ~init:false |> fun s -> 
      let l,h = int_of_string l, int_of_string h in
      let s = ref s in
      Tjr_list.from_to l h |> List.iter (fun i ->
          insert i (2*i) |> run ~init_state:!s |> fun (_,s') -> s:=s');
      close !s)


  | ["nop"] -> (
      (* print_endline "nop ok" *)
    )

  | _ ->
    failwith (
      Printf.sprintf "Unrecognized args: %s, at %s"
        (String_.concat_strings ~sep:" " args)
        __LOC__)

let _ = main (Sys.argv |> Array.to_list |> List.tl)

