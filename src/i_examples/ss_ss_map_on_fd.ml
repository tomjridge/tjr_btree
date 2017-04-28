(* a small KV store; keys and values are <=256 bytes *)

(*
FIXME have a single file with kv example ops and pp eg int_int_params

then check to see that the example works before fixing up testing
*)

open Prelude
open Btree_api
open Small_string
open Example_keys_and_values

module G = Generic_kv_store

let pp = ss_ss_pp

let cs sz = Constants.make_constants sz 4 pp.k_len pp.v_len

(* TODO use recycling store *)

let mk_ps1 sz = G.mk_ps1 (cs sz) Small_string.compare ss_ss_pp

let mk_unchecked_map_ops sz = G.mk_unchecked_map_ops (mk_ps1 sz)  (* FIXME unchecked *)


(* instantiate ---------------------------------------- *)

let sz = 4096

let map_ops = mk_unchecked_map_ops sz

let ls_ops = G.mk_ls_ops (mk_ps1 sz)

let main args = (
  (* turn off wf checking *)
  Test.disable ();
  match args with
  | ["init"; fn] -> (
      G.from_file ~fn ~create:true ~init:true ~pp
      |> (fun t -> print_endline "init ok"))
  | ["insert";fn;k;v] -> (
      let t = G.from_file ~fn ~create:false ~init:false ~pp in
      map_ops.insert (SS_.of_string k) (SS_.of_string v) 
      |> (fun f -> f t |> function | (t,Ok _) -> (
            G.write_root_block t)))
  | ["delete";fn;k] -> (
      let t = G.from_file ~fn ~create:false ~init:false ~pp in
      map_ops.delete (SS_.of_string k) 
      |> (fun f -> f t |> function | (t,Ok _) -> (
            G.write_root_block t)))
  | ["list";fn] -> (
      let t = G.from_file ~fn  ~create:false ~init:false ~pp in
      all_kvs ls_ops 
      |> (fun f -> f t |> function | (_,Ok kvs) -> (
            List.iter (fun (k,v) -> 
                Printf.printf "%s -> %s\n" (SS_.to_string k) (SS_.to_string v)) kvs);
          print_endline "list ok"))
  | _ -> (
      failwith ("Unrecognized args: "^(Tjr_string.concat_strings " " args)^ __LOC__))
)
