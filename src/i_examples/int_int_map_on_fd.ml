(* a map from int to int, backed by file ------------------------------- *)
open Prelude
open Btree_api
open Example_keys_and_values
open Btree_with_pickle.O
open Small_string.O
open Block.Blk4096

let r2t_ref = ref (fun x -> failwith "")

let ps = 
  let pp = int_int_pp in
  object
    method blk_sz=blk_sz
    method pp=pp
    method constants=Constants.make_constants blk_sz tag_len pp.k_len pp.v_len
    method cmp=Int_.compare
    method dbg_ps=Some(
        let f = (fun i -> `Int i) in
        object 
          method k2j=f
          method v2j=f
          method r2j=f
          method r2t=(!r2t_ref)
        end
      )
  end

open Map_on_fd
open Map_on_fd.Default_implementation

let store_ops = mk_store_ops ~ps ~ops

let _ = r2t_ref := Prelude.store_ops_to_r2t store_ops

let map_ops = 
  Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops

let ls_ops = mk_ls_ops ~ps ~page_ref_ops ~store_ops


let from_file ~fn ~create ~init = from_file ~fn ~create ~init ~ps

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
      |> function (t,Ok _) -> (close ~blk_sz t))
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false
      |> map_ops.delete (int_of_string k) 
      |> function (t,Ok _) -> (close ~blk_sz t))
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false
      |> (fun s -> 
          s 
          |> all_kvs ls_ops 
          |> (function (s',Ok kvs) -> (
                (List.iter (fun (k,v) -> 
                     Printf.printf "%s -> %s\n" (string_of_int k) 
                       (string_of_int v)) kvs);
                close ~blk_sz s';
                ()));                
          print_endline "list ok"))
  | _ -> (
      failwith ("Unrecognized args: "^(String_.concat_strings " " args)^ 
                __LOC__))
)
