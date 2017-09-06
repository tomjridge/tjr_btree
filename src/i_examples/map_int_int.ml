(* a map from int to int, backed by file ------------------------------- *)

open Examples_common
open Bin_prot_util

let read = Bin_prot.Std.bin_reader_int
let write = Bin_prot.Std.bin_writer_int
let sz = bp_size_int

(* this is generally useful, not just in examples *)
let ps' ~blk_sz = 
  Binprot_marshalling.mk_binprot_ps ~blk_sz
    ~cmp:Int_.compare ~k_size:sz ~v_size:sz
    ~read_k:read ~write_k:write
    ~read_v:read ~write_v:write

let ps = ps' ~blk_sz

let x = mk_example_on_fd ~ps

let from_file = from_file x
let map_ops = map_ops x
let close = close x
let ls_ops = ls_ops x



(* a map from int to int, backed by file ------------------------------- *)

open Map_ops


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
      |> insert (int_of_string k) (int_of_string v) 
      |> function (t,Ok _) -> (close t))
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false
      |> delete (int_of_string k) 
      |> function (t,Ok _) -> (close t))
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false
      |> (fun s -> 
          s 
          |> Leaf_stream_util.all_kvs ~ls_ops 
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
