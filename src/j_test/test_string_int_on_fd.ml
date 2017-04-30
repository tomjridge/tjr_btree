open Prelude
open Btree_api
open Page_ref_int
open Default
open Simple_monad
open Small_string
(* FIXME too many opens *)

open Ss_int_map_on_fd

let map_ops : (SS_.t,int,Map_on_fd.t) map_ops = mk_checked_map_ops sz

(* TODO use exhaustive *)
let test () = (
  Printf.printf "%s: " __MODULE__;
  flush_out();
  let s = Map_on_fd.from_file
      ~fn:default_filename ~create:true ~init:true ~sz:default_size ~pp:pp 
  in
  let s = ref s in
  let xs = ref Test_common.strings in
  let c = ref 1 in
  let m = ref Map_string.empty in
  ignore (
    while (!xs <> []) do
      print_string "."; flush_out();
      let (k,v) = (List.hd !xs, !c) in
      (* log __LOC__;
         log (Printf.sprintf "insert: %s %s" k (v|>string_of_int)); *)
      ignore (map_ops.insert (SS_.of_string k) v 
              |> run !s 
              |> (function (s',Ok ()) -> s:=s'));
      m:=(Map_string.add k v !m);
      c:=!c+1;
      xs:=(List.tl !xs);
      ()
    done);
  (* check the bindings match *)
  ignore (!m|>Map_string.bindings|>List.iter (fun (k,v) ->
      ignore (map_ops.find (SS_.of_string k) 
              |> run !s 
              |> (function (_,Ok res) -> assert (res = Some v)));
      ignore (map_ops.delete (SS_.of_string k) 
              |> run !s 
              |> function (s',Ok ()) -> s:=s');
      ()));
  Unix.close (!s).fd;
  ())

