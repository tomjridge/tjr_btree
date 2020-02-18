(** A simple example of a kv store. *)
open Tjr_monad.With_lwt

(* FIXME put in tjr_profile *)
module Profile() = struct
  let t1 = ref 0
  let t2 = ref 0
  let start () = t1:= Tjr_profile.now()
  let stop () = t2:= Tjr_profile.now()
  let print s = Printf.printf "%s took: %d\n%!" s (!t2 - !t1)
end

module Make(S:sig
    type btree_descr
    type k
    type v
    type r = Blk_id_as_int.blk_id
    type t = lwt
    type leaf_stream
    val show_cache: btree_descr -> (unit,t)m (* FIXME only for debugging *)
    val flush_cache: btree_descr -> (unit,t)m
    val map_ops_with_ls: btree_descr -> (k,v,r,leaf_stream,t)Map_ops_with_ls.map_ops_with_ls
    val int_to_k: int -> k
    val int_to_v: int -> v
  end)
=
struct
  open S

  (* FIXME config *)
  let max_writes = 10000

  let ops = map_ops_with_ls  

  let profile s (f:unit -> (unit,lwt)m) = 
    let module P = Profile() in
    let open P in
    return () >>= fun () ->
    start ();
    f () >>= fun () ->
    stop ();
    print s;
    return ()

  (* create and init store, write some values, and close *)
  let do_write bd = profile "do_write" begin
      fun () -> 
        Printf.printf "Executing %d writes...\n%!" max_writes;
        print_endline "Writing...";
        1 |> iter_k (fun ~k x ->
            match x > max_writes with
            | true -> return ()
            | false -> 
              (ops bd).insert ~k:(int_to_k x) ~v:(int_to_v x) >>= fun () -> 
              k (x+1)) >>= fun () ->
        show_cache bd >>= fun () ->
        (* Printf.printf "Before write flush\n"; *)
        flush_cache bd >>= fun () -> 
        (* Printf.printf "After write flush\n"; *)
        show_cache bd
        
    end

  let _ = do_write 

  (* delete some values *)
  let do_delete bd = profile "do_delete" begin
      fun () -> 
        print_endline "Deleting...";
        100 |> iter_k (fun ~k x -> 
            match x > 200 with
            | true -> return ()
            | false -> 
              (* Printf.printf "Deleting %d\n" x; *)
              (ops bd).delete ~k:(int_to_k x) >>= fun () ->
              show_cache bd >>= fun () ->
              k (x+1)) >>= fun () ->
        (* Printf.printf "About to flush\n"; *)
        flush_cache bd >>= fun () -> 
        (* Printf.printf "Post flush\n"; *)
        return ()
    end

  (* open store and check whether various keys and values are correct *)
  let do_check bd = 
    print_endline "Checking...";
    (ops bd).find ~k:(int_to_k 100) >>= fun v ->
    assert(v=None);
    (ops bd).find ~k:(int_to_k 1000) >>= fun v -> 
    assert(v = Some (int_to_v 1000));
    return ()

  let do_full_check bd = profile "do_full_check" @@ fun () -> 
    print_endline "Full check...";
    1 |> iter_k (fun ~k x ->
        match x > max_writes with
        | true -> return ()
        | false ->  
          (* Printf.printf "full_check: %d\n%!" x; *)
          (ops bd).find ~k:(int_to_k x) >>= fun v -> 
          assert( (100 <= x && x <= 200 && v=None) || v=Some(int_to_v x));
          k (x+1))

  (* actually execute the above *)
  let do_all bd = 
    do_write bd >>= fun () ->
    do_delete bd >>= fun () ->
    (* do_check bd >>= fun () -> *)
    do_full_check bd >>= fun () ->
    flush_cache bd (* not needed? *)
end


type arg = 
  | A1_int_int

type res = 
  | R1 of (unit -> (unit,lwt)m)

let filename = "btree.store"

let make_1 () = 
  let module X = Examples.Make_1() in
  let open X in
  open_ ~flag:Init_empty filename >>= fun bd ->
  let module Z = struct
    type btree_descr=X.t
    type k = int
    type v = int
    type r = Blk_id_as_int.blk_id
    type t = lwt
    type leaf_stream = X.Btree.leaf_stream
    let map_ops_with_ls = map_ops_with_ls
    let int_to_k: int -> k = fun x -> x
    let int_to_v: int -> v = fun x -> x
    let flush_cache = flush_cache
    let show_cache = show_cache
  end
  in
  let module W = Make(Z) in
  W.do_all bd >>= fun () ->
  close bd

let _ = make_1

