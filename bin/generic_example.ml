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
    type r = Blk_id_as_int.blk_id
    type t = lwt
    type leaf_stream
    val flush_cache: unit -> (unit,t)m
    val map_ops_with_ls:(k,v,r,leaf_stream,t)Map_ops_with_ls.map_ops_with_ls
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
  let do_write () = profile "do_write" @@ fun () -> 
    Printf.printf "Executing %d writes...\n%!" max_writes;
    print_endline "Writing...";
    1 |> iter_k (fun ~k x ->
        match x > max_writes with
        | true -> return ()
        | false -> 
          ops.insert ~k:(int_to_k x) ~v:(int_to_v x) >>= fun () -> 
          k (x+1)) >>= fun () ->
    flush_cache()

  let _ = do_write 

  (* delete some values *)
  let do_delete () = profile "do_delete" @@ fun () -> 
    print_endline "Deleting...";
    100 |> iter_k (fun ~k x -> 
        match x > 200 with
        | true -> return ()
        | false -> 
          ops.delete ~k:(int_to_k x) >>= fun () ->
          k (x+1)) >>= fun () ->
    flush_cache()

  (* open store and check whether various keys and values are correct *)
  let do_check () = 
    print_endline "Checking...";
    ops.find ~k:(int_to_k 100) >>= fun v ->
    assert(v=None);
    ops.find ~k:(int_to_k 1000) >>= fun v -> 
    assert(v = Some (int_to_v 1000));
    return ()

  let do_full_check () = profile "do_full_check" @@ fun () -> 
    print_endline "Full check...";
    1 |> iter_k (fun ~k x ->
        match x > max_writes with
        | true -> return ()
        | false -> 
          ops.find ~k:(int_to_k x) >>= fun v -> 
          assert( (100 <= x && x <= 200 && v=None) || v=Some(int_to_v x));
          k (x+1))

  (* actually execute the above *)
  let do_all() = 
    do_write () >>= fun () ->
    do_delete () >>= fun () ->
    do_check () >>= fun () ->
    do_full_check() >>= fun () ->
    flush_cache() (* not needed *)
end


type arg = 
  | A1_int_int

type res = 
  | R1 of (unit -> (unit,lwt)m)


let filename = "btree.store"

let make_1 () = 
  let module X = Examples.Make_1() in
  X.mk_blk_dev_ops ~filename >>= fun m ->   
  let module M = (val m) in
  let open M in
  let module Y = X.Make_2(struct let blk_dev_ops = blk_dev end) in
  Y.initialize_blk_dev () >>= fun () ->
  let module Z = struct
    type k = int
    type v = int
    type r = Blk_id_as_int.blk_id
    type t = lwt
    type leaf_stream = X.Btree.leaf_stream
    let map_ops_with_ls:(k,v,r,leaf_stream,t)Map_ops_with_ls.map_ops_with_ls = Y.map_ops_with_ls
    let int_to_k: int -> k = fun x -> x
    let int_to_v: int -> v = fun x -> x
    let flush_cache = Y.flush_cache
  end
  in
  let module W = Make(Z) in
  W.do_all () >>= fun () ->
  M.close_blk_dev ()

let _ = make_1
  



(* FIXME add this stuff back in
let make_generic_main ~fn ~int_to_k ~int_to_v ~example = 
  let Examples.{monad_ops;blk_ops;empty_leaf_as_blk; blk_dev_ops; blk_allocator_ref; btree_root_ref; flush_wbc; _} = example in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { from_file; close } =
    Blk_layer_2.make_from_file_and_close ~monad_ops ~blk_ops
    ~empty_leaf_as_blk in
  from_file ~fn ~create:true ~init:true >>= fun (fd,ba_root,bt_root) -> 
  let run = {run=Tjr_monad.Imperative.of_m} in
  btree_root_ref := bt_root;
  blk_allocator_ref := ba_root; 
  let map_ops_with_ls = example.map_ops_with_ls ~note_cached:() fd in
  let example = make_generic_example
      ~map_ops_with_ls
      ~run 
      ~int_to_k ~int_to_v
  in
  example.do_all ();
  run.run (flush_wbc ~blk_dev_ops:(blk_dev_ops fd) ());
  close ~fd ~blk_allocator_state:!blk_allocator_ref ~btree_root_state:!btree_root_ref
*)
