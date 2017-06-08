(* a map from int to int, backed by file ------------------------------- *)

(* for performance reasons, we use custom marshalling *)

open Prelude
open Btree_api
open Example_keys_and_values
(* open Btree_with_pickle_ *)
open Small_string.O
open Block.Blk4096
open Frame
open Page_ref_int

module Blk = Block.Blk4096

module BP = Bin_prot

open BP.Std

type iis = N of int list * int list | L of (int*int) list [@@deriving bin_io]

let f2iis frm = (
  match frm with
  | Node_frame (ks,rs) -> N (ks,rs)
  | Leaf_frame kvs -> L kvs)

let iis2f iis = (
  match iis with
  | N (ks,rs) -> Node_frame(ks,rs)
  | L kvs -> Leaf_frame kvs)

open Bigarray
type buf = BP.Common.buf

let frm_to_pg blk_sz (frm:(int,int)frame) = (
  let buf = BP.Common.create_buf blk_sz in
  let pos' = frm |> f2iis |> bin_writer_iis.write buf ~pos:0 in
  let s = Bytes.create blk_sz in
  let () = BP.Common.blit_buf_string buf s pos' in
  s |> Blk.of_string
)

let pg_to_frm pg = (
  let s = pg |> Blk.to_string in
  let buf = BP.Common.create_buf blk_sz in
  let _ = BP.Common.blit_string_buf s buf blk_sz in
  let iis : iis = bin_reader_iis.read buf (ref 0) in
  iis|>iis2f)

let ps = 
  object
    method blk_sz=blk_sz
    method page_to_frame=pg_to_frm
    method frame_to_page=frm_to_pg
    method cmp=Int_.compare
    method constants=Constants.make_constants blk_sz 4 4 4 (* TODO not correct - need min and max size that can fit using bin_prot *)
    method dbg_ps=None
  end

let main args = (
  (* turn off wf checking *)
  Examples_common.mk_example ~ps ~kk:(
    fun ~disk_ops ~store_ops ~map_ops ~imperative_map_ops ~ls_ops 
      ~from_file ~close -> 
      Test.disable ();
      match args with
      | ["init"; fn] -> (
          from_file ~fn ~create:true ~init:true
          |> (fun _ -> print_endline "init ok"))
      | ["insert";fn;k;v] -> (
          from_file ~fn ~create:false ~init:false
          |> map_ops.insert (int_of_string k) (int_of_string v) 
          |> function (t,Ok _) -> (close t))
      | ["delete";fn;k] -> (
          from_file ~fn ~create:false ~init:false
          |> map_ops.delete (int_of_string k) 
          |> function (t,Ok _) -> (close t))
      | ["list";fn] -> (
          from_file ~fn  ~create:false ~init:false
          |> (fun s -> 
              s 
              |> all_kvs ls_ops 
              |> (function (s',Ok kvs) -> (
                    (List.iter (fun (k,v) -> 
                         Printf.printf "%s -> %s\n" (string_of_int k) 
                           (string_of_int v)) kvs);
                    close s';
                    ()));                
              print_endline "list ok"))
      | _ -> (
          failwith ("Unrecognized args: "^(String_.concat_strings " " args)^ 
                    __LOC__)))
)
