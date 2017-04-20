(* a small KV store; keys and values are <=256 bytes *)

open Prelude
open Btree_api
open Map_prelude  (* FIXME move small_string to separate module, in /kv/ *)

module G = Generic_kv_store

module M_ = Map_string_string_small

(* TODO use recycling store *)
include G.Make_uncached (struct
    open G
    type k = M_.KV.key
    type v = M_.KV.value
    let ps = {
      pp=M_.pp;
      compare_k=M_.KV.key_ord;
      equal_v=M_.KV.equal_value;  (* FIXME eliminate this redundancy - just provide ps in map_xxx.ml *)
    }
  end)

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
      (* TODO need to get leaf_stream working properly *)
      |> (fun t -> 
          Btree_.Leaf_stream_.mk t.page_ref |> Sem.run t.store
          |> function (store,Ok ls) -> 
            Btree_.Leaf_stream_.all_kvs () |> Sem.run (t.store,ls) 
            |> function (_,Ok kvs) -> 
              (List.iter (fun (k,v) -> 
                   Printf.printf "%s -> %s\n" (SS.to_string k) (SS.to_string v)) kvs);
              print_endline "list ok"))
  | _ -> (failwith ("Unrecognized args: "^
                    (Tjr_string.concat_strings " " args)^
                    __LOC__))
)



