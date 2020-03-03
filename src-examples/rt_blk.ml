(** root block/open/close functionality *)
open Tjr_monad.With_lwt
module Blk_id = Blk_id_as_int
open Blk_id
open Intf_

open Intf_.Rt_blk_type

(** Instantiate Tjr_fs_shared.Rt_blk *)
module Root_block_ = Root_block.Make(struct type data = rt_blk[@@deriving bin_io] end)

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
    Root_block_.write_to_disk ~blk_dev_ops ~blk_id:b0 ~data:rbs >>= fun () ->
    (* finally, return the state *)
    return rbs
end
open Pvt

module type T = Intf_.FROM_OPEN

module B = Blk_id_as_int

let open_ ?flgs:(flgs=[]) ~empty_leaf_as_blk fn = 
  Blk_dev_factory.(make_8 fn) >>= fun x ->
  let module Blk_dev = (val x) in (* close_blk_dev is just close on fd *)
  let open Blk_dev in
  let maybe_truncate = 
    if List.mem O_TRUNC flgs then
      from_lwt (Lwt_unix.ftruncate fd 0)
    else return ()
  in
  maybe_truncate >>= fun () ->
  let maybe_init =
    if List.mem O_TRUNC flgs then
      initialize_blk_dev ~blk_dev_ops ~empty_leaf_as_blk
    else
      Root_block_.read_from_disk ~blk_dev_ops ~blk_id:b0
  in
  maybe_init >>= fun rt_blk ->
  let maybe_nocache = 
    match List.mem O_NOCACHE flgs with 
    | true -> (
        Printf.printf "Warning: O_NOCACHE not implemented"; return ())
    | false -> return ()
  in
  maybe_nocache >>= fun () ->
  let wrt_rt_and_close () = 
    let _ : unit = 
      if Debug_.debug_enabled then
        Printf.printf "Writing rt and closing: %d %d\n%!" (rt_blk.bt_rt |> (!) |> B.to_int) (rt_blk.blk_alloc |> (!) |> B.to_int)
    in
    Root_block_.write_to_disk ~blk_dev_ops ~blk_id:b0 ~data:rt_blk >>= fun () ->
    from_lwt (Lwt_unix.close Blk_dev.fd)
  in
  let module A = struct
    let fd = fd
    let blk_dev_ops = blk_dev_ops
    let rt_blk = rt_blk
    let wrt_rt_and_close = wrt_rt_and_close

    type r = blk_id
    let blk_alloc : (r,lwt) blk_allocator_ops = {
      blk_alloc=(fun () -> 
          let r = rt_blk.blk_alloc in
          assert(!r |> B.to_int >=0);
          let r' = !r in
          let _ : unit = 
            if Debug_.debug_enabled then
              Printf.printf "Allocating blk: %d\n%!" (r' |> B.to_int)
          in
          B.incr r;
          return r');
      blk_free=(fun _ -> return ())
    }

    let root_ops = 
      let with_state = fun f -> 
        f ~state:(!(rt_blk.bt_rt)) 
          ~set_state:(fun x -> 
              let _ : unit = 
                if Debug_.debug_enabled then 
                  Printf.printf "root_ops: set root %d\n%!" (B.to_int x)
              in
              rt_blk.bt_rt:=x; 
              return ())
      in
      {with_state}
  end
  in
  return (module A : T)
