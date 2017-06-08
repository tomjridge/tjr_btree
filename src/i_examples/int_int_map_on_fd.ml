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

let _ = assert (Sys.int_size = 63)  (* ensure we are on 64 bit system *)

let ps = 
  let int_size=5 in (* bin_prot marshalling an int *)
  let list_tag_size=5 in
  let n_or_l_tag_size=5 in
  let kv_size = 2*int_size in
  let kvs_in_blk = (blk_sz - n_or_l_tag_size - list_tag_size) / kv_size in 
  (* for nodes, the calculation is similar, except that we have two lists and one more r than k *)
  let max_node_keys = (blk_sz - n_or_l_tag_size - 2 * list_tag_size - int_size) / (2*int_size) in
  let constants=Constants.({min_leaf_size=2;max_leaf_size=kvs_in_blk;min_node_keys=2; max_node_keys}) in
  object
    method blk_sz=blk_sz
    method page_to_frame=pg_to_frm
    method frame_to_page=frm_to_pg
    method cmp=Int_.compare
    method constants=constants
    method dbg_ps=None
  end

open Examples_common

let x = mk_example ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x

let main args = (
  (* turn off wf checking *)
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
                __LOC__))
)
