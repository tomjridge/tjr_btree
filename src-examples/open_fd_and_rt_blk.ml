(** Functionality to open a store, and handle the root block *)

open Tjr_monad.With_lwt
open Intf_
module B = Blk_id_as_int

let open_ ~flgs ~fn = 
  from_lwt (Lwt_unix.openfile fn flgs Tjr_file.default_create_perm) >>= fun fd -> 
  Blk_dev_factory.(make_7 fd) |> fun (x : (module Blk_dev_factory.R6)) ->
  let module Blk_dev = (val x) in (* close_blk_dev is just close on fd *)
  let open Blk_dev in
  object
    method fd = fd
    method blk_dev_ops = blk_dev_ops
    method close_fd = close_blk_dev
  end 
  |> fun o -> return (o : open_fd)

let _ = open_

let rt_blk ~(open_fd:open_fd) : rt_blk = 
  let blk_dev_ops = open_fd#blk_dev_ops in
  let module A = struct
    (* open Bin_prot.Std *)
    let b0 = B.of_int 0
    type t = (* bt_rt *) B.blk_id * (* blk_alloc *)B.blk_id[@@deriving bin_io]
    let write_to_disk (t:t) = 
      let buf = ba_buf_ops.create 4096 in
      let _ : int = bin_write_t buf ~pos:0 t in
      blk_dev_ops.write ~blk_id:b0 ~blk:buf >>= fun () -> 
      let x,y = t in
      Printf.printf "Wrote roots to disk: %d %d\n%!" (B.to_int x) (B.to_int y);
      return ()
    let read_from_disk () = 
      blk_dev_ops.read ~blk_id:b0 >>= fun blk -> 
      bin_read_t blk ~pos_ref:(ref 0) |> fun (x,y) -> 
      Printf.printf "Read roots from disk: %d %d\n%!" (B.to_int x) (B.to_int y);
      return (x,y)
  end
  in
  let bt_rt_ref = ref (-1|>B.of_int) in
  let blk_alloc_ref = ref (-1|>B.of_int) in 
  let init_from_disk () = 
    (* read bt_rt and blk_alloc from disk *)
    A.read_from_disk () >>= fun (x,y) ->
    bt_rt_ref:=x;
    blk_alloc_ref:=y;
    return ()
  in
  let init_as_empty ~empty_leaf_as_blk =
    bt_rt_ref:=(1|>B.of_int);
    blk_alloc_ref:=(2|>B.of_int);
    blk_dev_ops.write 
      ~blk_id:(1|>B.of_int) 
      ~blk:(empty_leaf_as_blk()) >>= fun () -> 
    A.write_to_disk (!bt_rt_ref,!blk_alloc_ref) >>= fun () ->
    return ()
  in
  let with_bt_rt = with_ref bt_rt_ref in
  let blk_alloc : (B.blk_id,lwt) blk_allocator_ops = {
    blk_alloc=(fun () -> 
        let r = blk_alloc_ref in
        assert(!r |> B.to_int >=0); (* sanity check *)
        let r' = !r in
        let _ : unit = 
          if Debug_.debug_enabled then
            Printf.printf "Allocating blk: %d\n%!" (r' |> B.to_int)
        in
        B.incr r;
        return r');
    blk_free=(fun _ -> return ())
  }
  in
  let sync () = A.write_to_disk (!bt_rt_ref,!blk_alloc_ref) in
  object
    method init_from_disk = init_from_disk
    method init_as_empty = init_as_empty
    method blk_dev_ops = blk_dev_ops
    method with_bt_rt = with_bt_rt
    method blk_alloc = blk_alloc
    method sync = sync
  end |> fun o -> (o : rt_blk)
