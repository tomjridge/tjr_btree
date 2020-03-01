(** root block/open/close functionality *)
open Tjr_monad.With_lwt
module Blk_id = Blk_id_as_int
open Blk_id
open Intf_

open Bin_prot.Std

(** The specific rt_blk type, a pair of refs *)
module Rt_blk_type = struct
  type rt_blk = {
    bt_rt:blk_id ref;
    blk_alloc:blk_id ref
  }[@@deriving bin_io]    
end
open Rt_blk_type

(** Instantiate Tjr_fs_shared.Rt_blk *)
module Rt_blk_ = struct
  module X = Root_block.Make(struct type data = rt_blk[@@deriving bin_io] end)
  include X
end

module Pvt = struct
  let b0 = Blk_id.of_int 0 (* where we store the "superblock" *)
  let b1 = Blk_id.of_int 1 (* where we store the initial empty btree leaf node *)
  let b2 = Blk_id.of_int 2 (* first free blk *)

  (** Construct the initial root block state (rbs); write btree empty leaf; write rbs *)
  let initialize_blk_dev ~(blk_dev_ops:(_,_,_)blk_dev_ops) ~empty_leaf_as_blk =
    (* initial root-block state: bt_rt -> 1; blk_alloc -> 2 *)
    let rbs = { bt_rt=ref b1; blk_alloc=ref b2 } in
    (* initialize the btree *)
    blk_dev_ops.write ~blk_id:b1 ~blk:(empty_leaf_as_blk ()) >>= fun () ->
    (* sync rt_blk to b0 *)
    Rt_blk_.write_to_disk ~blk_dev_ops ~blk_id:b0 ~data:rbs >>= fun () ->
    (* finally, return the state *)
    return rbs
end
open Pvt

module type T = 
sig
  val fd          : Lwt_unix.file_descr
  val blk_dev_ops : (blk_id, ba_buf,lwt)blk_dev_ops
  val rt_blk      : rt_blk

  (** Close writes the root blk, then closes the fd; NOTE it does not flush the cache *)
  val close       : unit -> (unit, lwt)m

  val blk_alloc   : (blk_id, lwt) blk_allocator_ops
  val root_ops    : (blk_id, lwt) with_state
end

module B = Blk_id_as_int

let open_ ?flgs:(flgs=[]) ~empty_leaf_as_blk fn = 
  Blk_dev_factory.(make_8 fn) >>= fun x ->
  let module Blk_dev = (val x) in (* close_blk_dev is just close on fd *)
  let open Blk_dev in
  let maybe_truncate = 
    match List.mem O_TRUNC flgs with
    | true -> 
      (* truncate file *)
      from_lwt (Lwt_unix.ftruncate fd 0) >>= fun () ->
      initialize_blk_dev ~blk_dev_ops ~empty_leaf_as_blk
    | false -> 
      Rt_blk_.read_from_disk ~blk_dev_ops ~blk_id:b0
  in
  maybe_truncate >>= fun rt_blk ->
  let maybe_nocache = 
    match List.mem O_NOCACHE flgs with 
    | true -> (
        Printf.printf "Warning: O_NOCACHE not implemented"; return ())
    | false -> return ()
  in
  maybe_nocache >>= fun () ->
  let close () = 
    Rt_blk_.write_to_disk ~blk_dev_ops ~blk_id:b0 ~data:rt_blk >>= fun () ->
    from_lwt (Lwt_unix.close Blk_dev.fd)
  in
  let module A = struct
    let fd = fd
    let blk_dev_ops = blk_dev_ops
    let rt_blk = rt_blk
    let close = close

    type r = blk_id
    let blk_alloc : (r,lwt) blk_allocator_ops = {
      blk_alloc=(fun () -> 
          (* Printf.printf "Allocating blk: %d\n" (!blk_alloc_ref); *)
          let r = rt_blk.blk_alloc in
          assert(!r |> B.to_int >=0);
          let r' = !r in
          B.incr r;
          return r');
      blk_free=(fun _ -> return ())
    }

    let root_ops = with_ref rt_blk.bt_rt
  end
  in
  return (module A : T)
