open Monad
open Btree_api
open Page_ref_int
open Block.Blk4096
open Small_string
open Ss_int_map_on_fd
open Default_filename
(* FIXME too many opens *)

(* FIXME Default.fn etc *)

open Examples_common

let x = mk_example ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x

let (find,insert,delete) = 
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many -> 
  (find,insert,delete)

(* TODO use exhaustive; use imap_ops *)
let test () = (
  Printf.printf "%s: " __MODULE__;
  Base_types.flush_out();
  let s = from_file ~fn ~create:true ~init:true in
  let s = ref s in
  let xs = ref Test_common.strings in
  let c = ref 1 in
  let m = ref Map_string.empty in
  ignore (
    while (!xs <> []) do
      print_string "."; Base_types.flush_out();
      let (k,v) = (List.hd !xs, !c) in
      (* log __LOC__;
         log (Printf.sprintf "insert: %s %s" k (v|>string_of_int)); *)
      ignore (insert (SS.of_string k) v 
              |> run !s 
              |> (function (s',Ok ()) -> s:=s'));
      m:=(Map_string.add k v !m);
      c:=!c+1;
      xs:=(List.tl !xs);
      ()
    done);
  (* check the bindings match *)
  ignore (!m|>Map_string.bindings|>List.iter (fun (k,v) ->
      ignore (find (SS.of_string k) 
              |> run !s 
              |> (function (_,Ok res) -> assert (res = Some v)));
      ignore (delete (SS.of_string k) 
              |> run !s 
              |> function (s',Ok ()) -> s:=s');
      ()));
  close !s;
  ())

