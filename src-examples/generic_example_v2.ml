(** A simple example of a kv store. *)
open Tjr_monad.With_lwt
open Intf_
module B = Blk_id_as_int

(* FIXME put in tjr_profile *)
module Profile() = struct
  let t1 = ref 0
  let t2 = ref 0
  let start () = t1:= Tjr_profile.now()
  let stop () = t2:= Tjr_profile.now()
  let print s = Printf.printf "%s took: %d\n%!" s (!t2 - !t1)
end

module Make(S:sig
    type k
    type v
    type ls

    val btree    : (k,v,ls)btree
    val int_to_k : int -> k
    val int_to_v : int -> v
  end[@warning "-32"])
=
struct
  open S

  (* FIXME config *)
  let max_writes = 10000

  (* let ops = map_ops_with_ls   *)

  let profile s (f:unit -> (unit,lwt)m) = 
    let module P = Profile() in
    let open P in
    return () >>= fun () ->
    start ();
    f () >>= fun () ->
    stop ();
    print s;
    return ()

  let Map_ops_with_ls.{find; insert; delete; _} = btree#map_ops

  let flush_cache = btree#flush_cache

  (* create and init store, write some values, and close *)
  let do_write () = profile "do_write" begin
      fun () -> 
        Printf.printf "Executing %d writes...\n%!" max_writes;
        Printf.printf "Writing...\n%!";
        1 |> iter_k (fun ~k x ->
            match x > max_writes with
            | true -> return ()
            | false -> 
              insert ~k:(int_to_k x) ~v:(int_to_v x) >>= fun () -> 
              k (x+1)) >>= fun () ->
        (* show_cache bd >>= fun () -> *)
        Printf.printf "Before write flush\n";
        flush_cache () >>= fun () -> 
        Printf.printf "After write flush\n";
        (* show_cache bd *)
        Printf.printf "Finished writing\n%!";
        return ()
    end

  let _ = do_write 

  (* delete some values *)
  let do_delete () = profile "do_delete" begin
      fun () -> 
        Printf.printf "Deleting...\n%!";
        100 |> iter_k (fun ~k x -> 
            match x > 200 with
            | true -> return ()
            | false -> 
              (* Printf.printf "Deleting %d\n" x; *)
              delete ~k:(int_to_k x) >>= fun () ->
              (* show_cache bd >>= fun () -> *)
              k (x+1)) >>= fun () ->
        (* Printf.printf "About to flush\n"; *)
        flush_cache () >>= fun () -> 
        (* Printf.printf "Post flush\n"; *)
        Printf.printf "Finished deleting\n%!";
        return ()
    end

  (* open store and check whether various keys and values are correct *)
  let do_check () = 
    Printf.printf "Checking...\n%!";
    find ~k:(int_to_k 100) >>= fun v ->
    assert(v=None);
    find ~k:(int_to_k 1000) >>= fun v -> 
    assert(v = Some (int_to_v 1000));
    Printf.printf "Finished checking\n%!";
    return ()

  let do_full_check () = profile "do_full_check" @@ fun () -> 
    Printf.printf "Full check...\n%!";
    1 |> iter_k (fun ~k x ->
        match x > max_writes with
        | true -> return ()
        | false ->  
          (* Printf.printf "full_check: %d\n%!" x; *)
          find ~k:(int_to_k x) >>= fun v -> 
          assert( (100 <= x && x <= 200 && v=None) || v=Some(int_to_v x));
          k (x+1)) >>= fun () ->
    Printf.printf "Finished full check\n%!";
    return ()


  (* actually execute the above *)
  let do_all () = 
    do_write () >>= fun () ->
    do_delete () >>= fun () ->
    (* do_check bd >>= fun () -> *)
    do_full_check () >>= fun () ->
    flush_cache () (* not needed? *)
end


(* open B *)

type r = B.blk_id
    
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

let rt_blk ~open_fd : rt_blk = 
  let blk_dev_ops = open_fd#blk_dev_ops in
  let module A = struct
    (* open Bin_prot.Std *)
    let b0 = B.of_int 0
    type t = (* bt_rt *) B.blk_id * (* blk_alloc *)B.blk_id[@@deriving bin_io]
    let write_to_disk (t:t) = 
      let buf = ba_buf_ops.create 4096 in
      let _ : int = bin_write_t buf ~pos:0 t in
      blk_dev_ops.write ~blk_id:b0 ~blk:buf >>= fun () -> 
      return ()
    let read_from_disk () = 
      blk_dev_ops.read ~blk_id:b0 >>= fun blk -> 
      bin_read_t blk ~pos_ref:(ref 0) |> fun t -> 
      return t
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
  object
    method init_from_disk = init_from_disk
    method init_as_empty = init_as_empty
    method blk_dev_ops = blk_dev_ops
    method with_bt_rt = with_bt_rt
    method blk_alloc = blk_alloc
    method sync = fun () -> A.write_to_disk (!bt_rt_ref,!blk_alloc_ref)
  end |> fun o -> (o : rt_blk)

let fn = "example.store"

let make () = 
  let module Ex = Make_1.Int_int_ex in
  let open Ex in
  Lwt_unix.(open_ ~flgs:[O_CREAT;O_TRUNC;O_RDWR] ~fn) >>= fun open_fd ->
  let rt_blk = rt_blk ~open_fd in
  rt_blk#init_as_empty ~empty_leaf_as_blk >>= fun () -> 
  let blk_dev_ops,blk_alloc,root_ops = 
    open_fd#blk_dev_ops,rt_blk#blk_alloc,rt_blk#with_bt_rt
  in
  let module Map_ops_and_flush = 
    (* FIXME ex.make could just return btree directly *)
    (val Ex.make ~blk_dev_ops ~blk_alloc ~root_ops) in
  let btree : (_,_,_) btree = Map_ops_and_flush.(
      object
        method map_ops = map_ops_with_ls
        method flush_cache = flush_cache
      end)
  in
  let module S = struct
    include Ex
    let btree = btree
    let int_to_k: int -> k = fun x -> x
    let int_to_v: int -> v = fun x -> x
  end
  in
  let module T = Make(S) in
  T.do_all () >>= fun () ->
  btree#flush_cache() >>= fun () ->
  rt_blk#sync () >>= fun () ->
  open_fd#close_fd ()

let _ = make

