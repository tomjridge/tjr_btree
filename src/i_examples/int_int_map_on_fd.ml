(* a map from int to int, backed by file ------------------------------- *)

open Prelude
open Btree_api
open Frame
open Page_ref_int
open Examples_common

module Int_ = Bin_prot_int

let ps = 
  Binprot_marshalling.mk_ps ~blk_sz:4096 
    ~cmp:Int_.compare ~k_size:Int_.size ~v_size:Int_.size
    ~read_k:Int_.bin_reader_t ~write_k:Int_.bin_writer_t
    ~read_v:Int_.bin_reader_t ~write_v:Int_.bin_writer_t

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
      failwith (
        "Unrecognized args: "^
        (String_.concat_strings " " args)^ 
        __LOC__)) )
