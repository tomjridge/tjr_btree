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

let fn = "example.store"

open Open_fd_and_rt_blk

let make () = 
  let module Ex = Make_2.Int_int_ex in
  let open Ex in
  Lwt_unix.(open_ ~flgs:[O_CREAT;O_TRUNC;O_RDWR] ~fn) >>= fun open_fd ->
  let rt_blk = rt_blk ~open_fd in
  rt_blk#init_as_empty ~empty_leaf_as_blk >>= fun () -> 
  let btree = make_as_object ~open_fd ~rt_blk in
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
  open_fd#close ()


