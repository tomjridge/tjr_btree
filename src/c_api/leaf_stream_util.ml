open Monad
open Leaf_stream_ops

(* for debugging *)

(** Get all (key,value) pairs from a leaf stream. Debugging only. *)
let all_kvs ~ls_ops : (('k * 'v) list,'t) m = 
  dest_ls_ops ls_ops @@ fun ~mk_leaf_stream ~ls_step ~ls_kvs ->
  let rec loop kvs s = (
    let kvs' = ls_kvs s in
    let kvs = kvs@kvs' in
    ls_step s |> bind (fun s' ->
        match s' with
        | None -> return kvs
        | Some s' -> loop kvs s'))
  in
  mk_leaf_stream () |> bind (fun s -> loop [] s)


