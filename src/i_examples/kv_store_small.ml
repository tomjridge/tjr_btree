(* a small KV store; keys and values are <=256 bytes *)

(*
FIXME have a single file with kv example ops and pp eg int_int_params

then check to see that the example works before fixing up testing
*)

open Prelude
open Btree_api
open Small_string

module G = Generic_kv_store

module X = Example_keys_and_values

(* TODO use recycling store *)
module Uncached = G.Make_uncached (struct
    open G
    type k = Small_string.t
    type v = Small_string.t
    let ps = {
      pp=X.ss_ss_pp;
      kv_ops={
        compare_k=Small_string.compare;
        equal_v=(=);  (* FIXME eliminate this redundancy - just provide ps in map_xxx.ml *)
      }
    }
  end)

open Uncached

let main args = (
  (* turn off wf checking *)
  Test.disable ();
  match args with
  | ["init"; fn] -> (
      from_file ~fn ~create:true ~init:true 
      |> (fun t -> print_endline "init ok"))
  | ["insert";fn;k;v] -> (
      let t = from_file ~fn ~create:false ~init:false in
      map_ops.insert (SS_.of_string k) (SS_.of_string v) 
      |> (fun f -> f t |> function | (_,Ok _) -> ()) )
  | ["delete";fn;k] -> (
      let t = from_file ~fn ~create:false ~init:false in
      map_ops.delete (SS_.of_string k) 
      |> (fun f -> f t |> function | (_,Ok _) -> ()))
  | ["list";fn] -> (
      let t = from_file ~fn  ~create:false ~init:false in
      Uncached.Api.all_kvs map_ops 
      |> (fun f -> f t |> function | (_,Ok kvs) -> (
            List.iter (fun (k,v) -> 
                Printf.printf "%s -> %s\n" (SS_.to_string k) (SS_.to_string v)) kvs);
          print_endline "list ok"))
  | _ -> (
      failwith ("Unrecognized args: "^(Tjr_string.concat_strings " " args)^ __LOC__))
)
