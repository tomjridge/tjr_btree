(* a small KV store; keys and values are <=256 bytes *)

open Prelude
open Btree_api
open Frame
open Page_ref_int
open Examples_common
open Small_string
open Bin_prot_util

let read_k = bin_reader_ss
let write_k = bin_writer_ss
let read_v = bin_reader_ss
let write_v = bin_writer_ss


let ps' ~blk_sz = Binprot_marshalling.mk_ps ~blk_sz
    ~cmp:SS.compare ~k_size:bp_size_ss ~v_size:bp_size_ss
    ~read_k ~write_k
    ~read_v ~write_v

let ps = ps' ~blk_sz

let x = mk_example ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x

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
      |> insert (SS.of_string k) (SS.of_string v) 
      |> function (t,Ok _) -> (close t))
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false
      |> delete (SS.of_string k) 
      |> function (t,Ok _) -> (close t))
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false
      |> (fun s -> 
          s 
          |> all_kvs ~ls_ops 
          |> (function (s',Ok kvs) -> (
                (List.iter (fun (k,v) -> 
                     Printf.printf "%s -> %s\n" (SS.to_string k) 
                       (SS.to_string v)) kvs);
                close s';
                ()));                
          print_endline "list ok"))
  | _ -> (
      failwith ("Unrecognized args: "^(String_.concat_strings " " args)^ 
                __LOC__))
)
