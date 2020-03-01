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
    (* type ls *)

    val find        : k:k -> (v option, lwt) m
    val insert      : k:k -> v:v -> (unit, lwt) m
    val insert_many : k:k -> v:v -> kvs:(k * v) list -> ((k * v) list, lwt) m[@@warning "-32"]
    val insert_all  : kvs:(k * v) list -> (unit, lwt) m
    val delete      : k:k -> (unit, lwt) m
    (* val ls_create   : unit -> (ls, lwt) m *)
    (* val ls_step     : ls:ls -> (ls option, lwt) m *)
    (* val ls_kvs      : ls:ls -> (k * v) list *)
    val flush_cache : unit -> (unit,lwt)m
    val int_to_k    : int -> k
    val int_to_v    : int -> v
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
  let do_write = profile "do_write" begin
      fun () -> 
        Printf.printf "Executing %d writes...\n%!" max_writes;
        print_endline "Writing...";
        1 |> iter_k (fun ~k x ->
            match x > max_writes with
            | true -> return ()
            | false -> 
              insert ~k:(int_to_k x) ~v:(int_to_v x) >>= fun () -> 
              k (x+1)) >>= fun () ->
        (* show_cache bd >>= fun () -> *)
        (* Printf.printf "Before write flush\n"; *)
        flush_cache () >>= fun () -> 
        (* Printf.printf "After write flush\n"; *)
        (* show_cache bd *)
        return ()
    end

  let _ = do_write 

  (* delete some values *)
  let do_delete = profile "do_delete" begin
      fun () -> 
        print_endline "Deleting...";
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
        return ()
    end

  (* open store and check whether various keys and values are correct *)
  let do_check () = 
    print_endline "Checking...";
    find ~k:(int_to_k 100) >>= fun v ->
    assert(v=None);
    find ~k:(int_to_k 1000) >>= fun v -> 
    assert(v = Some (int_to_v 1000));
    return ()

  let do_full_check = profile "do_full_check" @@ fun () -> 
    print_endline "Full check...";
    1 |> iter_k (fun ~k x ->
        match x > max_writes with
        | true -> return ()
        | false ->  
          (* Printf.printf "full_check: %d\n%!" x; *)
          find ~k:(int_to_k x) >>= fun v -> 
          assert( (100 <= x && x <= 200 && v=None) || v=Some(int_to_v x));
          k (x+1))

  (* actually execute the above *)
  let do_all () = 
    do_write >>= fun () ->
    do_delete >>= fun () ->
    (* do_check bd >>= fun () -> *)
    do_full_check >>= fun () ->
    flush_cache () (* not needed? *)
end


let filename = "example.store"

let make () = 
  let module X = Tjr_btree_examples.Int_int_ex in
  (* let open X in *)
  Tjr_btree_examples.Rt_blk.open_ 
    ~flgs:[O_TRUNC] ~empty_leaf_as_blk:X.empty_leaf_as_blk filename >>= fun rbs ->
  let module Rbs = (val rbs) in
  let open Rbs in
  let module X2 = (val X.make ~blk_dev_ops ~blk_alloc ~root_ops) in
  let open X2 in
  let module Z = struct
    include X
    let Btree_intf.Map_ops_with_ls.{find;insert;insert_many;insert_all;delete;_} = 
      X2.map_ops_with_ls
    let flush_cache=flush_cache
    (* type t = lwt *)
    let int_to_k: int -> k = fun x -> x
    let int_to_v: int -> v = fun x -> x
  end
  in
  let module W = Make(Z) in
  W.do_all () >>= fun () ->
  close ()

let _ = make

