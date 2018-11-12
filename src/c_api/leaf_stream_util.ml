open Tjr_monad.Types
open Leaf_stream_ops

(* for debugging *)

(** Get all (key,value) pairs from a leaf stream. Debugging only. *)
let all_kvs (type r) ~monad_ops ~(ls_ops:('k,'v,r,'t)leaf_stream_ops) ~r : (('k * 'v) list,'t) m = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  dest_ls_ops ls_ops @@ fun ~mk_leaf_stream ~ls_step ~ls_kvs ->
  let rec loop kvs s = (
    let kvs' = ls_kvs s in
    let kvs = kvs@kvs' in
    ls_step s >>= fun s' ->
    match s' with
    | None -> return kvs
    | Some s' -> loop kvs s')
  in
  mk_leaf_stream r >>= (fun s -> loop [] s)


let _ = all_kvs
