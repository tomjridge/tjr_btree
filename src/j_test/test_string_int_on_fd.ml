open Monad
open Page_ref_int
open Block.Blk4096
open Small_string
open Ss_int_map_on_fd
open Default_filename
open Map_ops
(* FIXME too many opens *)

(* FIXME Default.fn etc *)

open Examples_common

let x = mk_example_on_fd ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x

let (find,insert,delete) = 
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many -> 
  (find,insert,delete)

(* TODO use exhaustive; use imap_ops *)
let test () = 
  Printf.printf "%s: " __MODULE__;
  Base_types.flush_out();
  let s = from_file ~fn ~create:true ~init:true in
  let s = ref s in
  (fun _ -> close !s) 
    begin
      let xs = ref Test_common.strings in
      let c = ref 1 in
      let m = ref Map_string.empty in
      ignore (
        while (!xs <> []) do
          print_string "."; Base_types.flush_out();
          let (k,v) = (List.hd !xs, !c) in
          Test.log (fun _ -> __LOC__);
          Test.log (fun _ -> Printf.sprintf "insert: %s %s" k (v|>string_of_int)); 
          ignore (insert (SS.of_string k) v 
                  |> run !s 
                  |> (function (s',Ok ()) -> s:=s'));
          m:=(Map_string.add k v !m);
          c:=!c+1;
          xs:=(List.tl !xs);
          ()
        done);
      (* check the bindings match *)
      ignore (
        !m|>Map_string.bindings|>List.iter (fun (k,v) ->
            let _ = 
              find (SS.of_string k)
              |> run !s 
              |> fun (_,Ok res) -> 
              Test.log (fun _ -> Printf.sprintf "testing key %s, expecting %s" k (string_of_int v));
              if res = Some v then () else (
                print_endline (Printf.sprintf "%s: key:%s expected %s, got %s" 
                                 __LOC__ 
                                 k
                                 (string_of_int v)
                                 (if res = None then "None" else 
                                    string_of_int (Base_types.dest_Some res)));
                Test.print_logs ();
                Pervasives.exit (-1))
            in
            let _ = 
              Test.log (fun _ -> Printf.sprintf "deleting key %s" k);
              delete (SS.of_string k) 
              |> run !s 
              |> fun (s',Ok ()) -> s:=s'
            in
            ()))
    end

