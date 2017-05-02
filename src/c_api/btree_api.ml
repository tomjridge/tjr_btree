(* various interfaces ---------------------------------------- *)

(* this module safe to open *)

open Prelude
open Default

(* block device ---------------------------------------- *)

(* constants are relevant; compare_k not relevant *)
type 't disk_ops = {
  block_size: BLK.sz;
  read: BLK.r -> (BLK.t,'t) m;
  write: BLK.r -> BLK.t -> (unit,'t) m;
  disk_sync: unit -> (unit,'t) m;
}


(* store ------------------------------------------------------------ *)

(* constants and compare_k not relevant *)
type ('k,'v,'r,'t) store_ops = ('k,'v,'r,'t) Store_ops.store_ops


(* map ------------------------------------------------------------ *)

type ('k,'v,'t) map_ops = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t) m;
}


(* we only reveal lss when it points to a leaf *)
type ('k,'v,'r) lss = { kvs: ('k*'v) list; ls: ('k,'v,'r)Isa_util.ls_state }

type ('k,'v,'r,'t) ls_ops = {
  mk_leaf_stream: unit -> (('k,'v,'r) lss,'t) m;
  ls_step: ('k,'v,'r) lss -> (('k,'v,'r) lss option,'t) m;
  ls_kvs: ('k,'v,'r) lss -> ('k*'v) list
}


(* for debugging *)
let all_kvs: ('k,'v,'r,'t)ls_ops -> (('k * 'v) list,'t) m = Simple_monad.(
    fun ops ->
      let rec loop kvs s = (
        let kvs' = ops.ls_kvs s in
        let kvs = kvs@kvs' in
        ops.ls_step s |> bind (fun s' ->
            match s' with
            | None -> return kvs
            | Some s' -> loop kvs s'))
      in
      ops.mk_leaf_stream () |> bind (fun s -> loop [] s))


(* fix page_ref --------------------------------------------------- *)

module Page_ref_int = struct
  type page_ref = int  [@@deriving yojson]
(*
  let dest_r (R i) = i
  let dest_rs rs = List.map dest_r rs
  let mk_r i = R i
  let mk_rs rs = List.map (fun x -> R x) rs
*)
  type ('k,'v) frame = ('k,'v,page_ref) Frame.frame
end

