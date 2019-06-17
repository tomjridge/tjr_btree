(** A simple example of a kv store. *)

open Examples
open Map_on_fd_util

let fn = 
  ref "btree.store"
  |> Global.register ~name:(__MODULE__^".fn (default: btree.store)")

(* Collect together vars that arise when creating a B-tree from a file on disk *)

type tmp = {
  fd:Unix.file_descr;
  root_block:root_block;
  fstore:Tjr_store.t ref;
  run:'a. ('a, fstore_passing) m -> 'a;
  close: unit -> unit;
}


let make_generic_example (type k v r leaf_stream) 
    ~on_disk_util
    ~(root_ops_to_map_ops_etc: 'root_ops -> (k,v,r,leaf_stream,fstore state_passing)Btree_intf.Map_ops_etc_type.map_ops_etc)
    ~int_to_k ~int_to_v
  =
  let module A = struct

    let {from_file;close} = on_disk_util

    (* FIXME config *)
    let max_writes = 10000



    (* Convert state passing to imperative style using an ocaml ref *)
    let run ~fstore (op:('a,fstore_passing)m) : 'a =
      State_passing.to_fun op (!fstore) |> fun (a,fstore') -> 
      fstore:=fstore';
      a


    (* We store various bits of state, including the B-tree root
       block, in the store *)

    let ba_ref,rb_ref,bd_ref = 
      Examples.(Blk_allocator.blk_allocator_ref,
                Btree_root_block.btree_root_block_ref,
                On_disk_blk_dev.blk_dev_ref)

    let root_ops = Examples.Btree_root_block.root_ops

    let map_ops_etc = root_ops_to_map_ops_etc root_ops



    (* Init and close *)

    let init_store ~fd ~(root_block:root_block) =
      (* Printf.printf "Init with free=%d and root=%d\n%!" root_block.free root_block.btree_root; *)
      let set = Tjr_store.set in
      Tjr_store.initial_store 
      |> set ba_ref {min_free_blk_id=root_block.free}
      |> set rb_ref root_block.btree_root
      |> set bd_ref (Some fd)

    let close ~fd ~fstore =
      let free = (Tjr_store.get ba_ref !fstore).min_free_blk_id in
      let btree_root = Tjr_store.get rb_ref !fstore in
      (* Printf.printf "Close with free=%d and root=%d\n%!" free btree_root; *)
      close ~fd ~root_block:{free; btree_root}


    let btree_from_file ~fn ~create ~init =
      let fd,root_block = from_file ~fn ~create ~init in
      let fstore = ref (init_store ~fd ~root_block) in
      let run x = run ~fstore x in
      let close () = close ~fd ~fstore in
      { fd; root_block; fstore; run; close }



    (* Some examples *)

    (* create and init store, write some values, and close *)
    let do_write () = 
      Printf.printf "Executing %d writes...\n%!" max_writes;
      print_endline "Writing...";
      btree_from_file ~fn:!fn ~create:true ~init:true |> fun { run; close; _ } -> 
      (* write values *)
      for x=1 to max_writes do
        let k,v = int_to_k x,int_to_v x in
        run (map_ops_etc.insert ~k ~v)
      done;
      close ();
      ()

    (* open store, delete some values, and close *)
    let do_delete () = 
      print_endline "Deleting...";
      btree_from_file ~fn:!fn ~create:false ~init:false |> fun { run; close; _ } -> 
      for x=100 to 200 do
        let k = int_to_k x in
        run (map_ops_etc.delete ~k);
      done;
      close ();
      ()

    (* open store and check whether various keys and values are correct *)
    let do_check () = 
      print_endline "Checking...";
      btree_from_file ~fn:!fn ~create:false ~init:false |> fun { run; close; _ } -> 
      assert(run (map_ops_etc.find ~k:(int_to_k 100)) = None);
      assert(run (map_ops_etc.find ~k:(int_to_k 1000)) = Some (int_to_v 1000));
      close ();
      ()

      (*
    let do_mini_check() = 
      do_write();
      do_delete();
      do_check();
      ()
*)
      
    let do_full_check () = 
      print_endline "Full check...";
      btree_from_file ~fn:!fn ~create:false ~init:false |> fun { run; close; _ } -> 
      for i = 1 to max_writes do
        let k = int_to_k i in
        if (100 <= i && i <= 200) then
          assert(run (map_ops_etc.find ~k) = None)
        else
          assert(run (map_ops_etc.find ~k) = Some(int_to_v i))
      done;
      close ();
      ()

    (* actually execute the above *)
    let do_all() = 
      do_write ();
      do_delete ();
      do_check ();
      do_full_check()
      
  end
  in
  A.(fun f -> f ~do_all ~btree_from_file ~map_ops_etc)

let _ = make_generic_example
