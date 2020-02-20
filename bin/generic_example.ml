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
    type k
    type v
    (* type r = Blk_id_as_int.blk_id *)
    (* type t = lwt *)
    type ls
    type bd

    val find         : bd:bd -> k:k -> (v option, lwt) m
    val insert       : bd:bd -> k:k -> v:v -> (unit, lwt) m
    val insert_many  : bd:bd -> k:k -> v:v -> kvs:(k * v) list -> ((k * v) list, lwt) m[@@warning "-32"]
    val insert_all   : bd:bd -> kvs:(k * v) list -> (unit, lwt) m
    val delete       : bd:bd -> k:k -> (unit, lwt) m
    val ls_create    : bd:bd -> (ls, lwt) m
    val ls_step      : bd:bd -> ls:ls -> (ls option, lwt) m
    val ls_kvs       : bd:bd -> ls:ls -> (k * v) list
    val sync_to_disk : bd:bd -> (unit, lwt) m
    val flush_cache  : bd:bd -> (unit, lwt) m
        
    val int_to_k: int -> k
    val int_to_v: int -> v
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

  (* create and init store, write some values, and close *)
  let do_write bd = profile "do_write" begin
      fun () -> 
        Printf.printf "Executing %d writes...\n%!" max_writes;
        print_endline "Writing...";
        1 |> iter_k (fun ~k x ->
            match x > max_writes with
            | true -> return ()
            | false -> 
              insert ~bd ~k:(int_to_k x) ~v:(int_to_v x) >>= fun () -> 
              k (x+1)) >>= fun () ->
        (* show_cache bd >>= fun () -> *)
        (* Printf.printf "Before write flush\n"; *)
        flush_cache ~bd >>= fun () -> 
        (* Printf.printf "After write flush\n"; *)
        (* show_cache bd *)
        return ()
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
              delete ~bd ~k:(int_to_k x) >>= fun () ->
              (* show_cache bd >>= fun () -> *)
              k (x+1)) >>= fun () ->
        (* Printf.printf "About to flush\n"; *)
        flush_cache ~bd >>= fun () -> 
        (* Printf.printf "Post flush\n"; *)
        return ()
    end

  (* open store and check whether various keys and values are correct *)
  let do_check bd = 
    print_endline "Checking...";
    find ~bd ~k:(int_to_k 100) >>= fun v ->
    assert(v=None);
    find ~bd ~k:(int_to_k 1000) >>= fun v -> 
    assert(v = Some (int_to_v 1000));
    return ()

  let do_full_check bd = profile "do_full_check" @@ fun () -> 
    print_endline "Full check...";
    1 |> iter_k (fun ~k x ->
        match x > max_writes with
        | true -> return ()
        | false ->  
          (* Printf.printf "full_check: %d\n%!" x; *)
          find ~bd ~k:(int_to_k x) >>= fun v -> 
          assert( (100 <= x && x <= 200 && v=None) || v=Some(int_to_v x));
          k (x+1))

  (* actually execute the above *)
  let do_all bd = 
    do_write bd >>= fun () ->
    do_delete bd >>= fun () ->
    (* do_check bd >>= fun () -> *)
    do_full_check bd >>= fun () ->
    flush_cache ~bd (* not needed? *)
end


type arg = 
  | A1_int_int

type res = 
  | R1 of (unit -> (unit,lwt)m)

let filename = "example.store"

let make_1 () = 
  let module X = Examples.Int_int_ex in
  let open X in
  open_ ~flgs:[O_TRUNC] filename >>= fun bd ->
  let module Z = struct
    (* type t = lwt *)
    include X
    let int_to_k: int -> k = fun x -> x
    let int_to_v: int -> v = fun x -> x
  end
  in
  let module W = Make(Z) in
  W.do_all bd >>= fun () ->
  close ~bd

let _ = make_1

