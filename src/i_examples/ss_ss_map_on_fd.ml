(* a small KV store; keys and values are <=256 bytes *)

(*
FIXME have a single file with kv example ops and pp eg int_int_params

then check to see that the example works before fixing up testing
*)

open Prelude
open Btree_api
open Small_string.O
open Btree_with_pickle.O
open Example_keys_and_values
open Default

let ps store_ops = 
  let pp = ss_ss_pp in
  object
    method blk_sz=blk_sz
    method pp=pp
    method constants=Constants.make_constants blk_sz tag_len pp.k_len pp.v_len
    method compare_k=SS.compare
    method debug=None (* TODO *)
    method store_ops=store_ops
  end

open Map_on_fd.Default_implementation

let store_ops = mk_store_ops (ps ())

let ps = ps store_ops

let map_ops = mk_map_ops ps

let imperative_map_ops = mk_imperative_map_ops ps

let ls_ops = mk_ls_ops ps

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
      |> map_ops.insert (SS.of_string k) (SS.of_string v) 
      |> function (t,Ok _) -> (close ~blk_sz t))
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false
      |> map_ops.delete (SS.of_string k) 
      |> function (t,Ok _) -> (close ~blk_sz t))
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false
      |> (fun s -> 
          s 
          |> all_kvs ls_ops 
          |> (function (s',Ok kvs) -> (
                (List.iter (fun (k,v) -> 
                     Printf.printf "%s -> %s\n" (SS.to_string k) 
                       (SS.to_string v)) kvs);
                close ~blk_sz s';
                ()));                
          print_endline "list ok"))
  | _ -> (
      failwith ("Unrecognized args: "^(Bt_string.concat_strings " " args)^ 
                __LOC__))
)
