(* a small KV store; keys and values are <=256 bytes *)

(*
FIXME have a single file with kv example ops and pp eg int_int_params

then check to see that the example works before fixing up testing
*)

open Prelude
open Btree_api
open Small_string
open Example_keys_and_values



module S = struct
  type k = Small_string.t
  type v = Small_string.t
  let pp = ss_ss_pp
  let sz = 4096
  let compare_k = Small_string.compare
end

module M = Map_on_fd.Make(S)  

include M  

(* TODO use recycling store *)

let map_ops = mk_unchecked_map_ops S.sz

open Map_on_fd
open S

let main args = (
  (* turn off wf checking *)
  Test.disable ();
  match args with
  | ["init"; fn] -> (
      from_file ~fn ~create:true ~init:true ~pp ~sz
      |> (fun (t:t) -> print_endline "init ok"))
  | ["insert";fn;k;v] -> (
      from_file ~fn ~create:false ~init:false ~pp ~sz 
      |> map_ops.insert (SS_.of_string k) (SS_.of_string v) 
      |> function (t,Ok _) -> (write_root_block sz t))
  | ["delete";fn;k] -> (
      from_file ~fn ~create:false ~init:false ~pp ~sz
      |> map_ops.delete (SS_.of_string k) 
      |> function (t,Ok _) -> (write_root_block sz t))
  | ["list";fn] -> (
      from_file ~fn  ~create:false ~init:false ~pp ~sz
      |> all_kvs ls_ops 
      |> function (_,Ok kvs) -> (
          List.iter (fun (k,v) -> 
              Printf.printf "%s -> %s\n" (SS_.to_string k) (SS_.to_string v)) kvs);
        print_endline "list ok")
  | _ -> (
      failwith ("Unrecognized args: "^(Tjr_string.concat_strings " " args)^ __LOC__))
)
