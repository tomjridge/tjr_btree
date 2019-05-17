(** A simple example of a kv store. *)

open Examples

let _ = 
  Printf.printf 
    "%s ----------------------------------------------\n%!" 
    __MODULE__

module Internal = struct
  let fn = ref "btree.store"

  let on_disk_util,(map_ops,{insert_many; leaf_stream_ops}) = 
    Examples.On_disk.Int_int.(on_disk_util,map)

  let from_file,close = on_disk_util

  let max = 10000

  (* TODO this would be much faster if we used insert_many *)

  let run ~fstore (op:('a,fstore_passing)m) : 'a =
    State_passing.to_fun op (!fstore) |> fun (a,fstore') -> 
    fstore:=fstore';
    a

  let ba_ref,rb_ref,bd_ref = 
    Examples.(Blk_allocator.blk_allocator_ref,
              Btree_root_block.btree_root_block_ref,
              On_disk_blk_dev.blk_dev_ref)


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

  type tmp = {
    fd:Unix.file_descr;
    root_block:root_block;
    fstore:Tjr_store.t ref;
    run:'a. ('a, fstore_passing) m -> 'a;
    close: unit -> unit;
  }

  let btree_from_file ~fn ~create ~init =
    let fd,root_block = from_file ~fn ~create ~init in
    let fstore = ref (init_store ~fd ~root_block) in
    let run x = run ~fstore x in
    let close () = close ~fd ~fstore in
    { fd; root_block; fstore; run; close }
end
open Internal

module Internal2 = struct
  (* create and init store, write some values, and close *)
  let do_write () = 
    Printf.printf "Executing %d writes...\n%!" max;
    print_endline "Writing...";
    btree_from_file ~fn:!fn ~create:true ~init:true |> fun { run; close; _ } -> 
    (* write values *)
    for x=1 to max do
      run (map_ops.insert ~k:x ~v:x)
    done;
    close ();
    ()

  (* open store, delete some values, and close *)
  let do_delete () = 
    print_endline "Deleting...";
    btree_from_file ~fn:!fn ~create:true ~init:true |> fun { run; close; _ } -> 
    for x=100 to 200 do
      run (map_ops.delete ~k:x);
    done;
    close ();
    ()

  (* open store and check whether various keys and values are correct *)
  let do_check () = 
    print_endline "Checking...";
    btree_from_file ~fn:!fn ~create:true ~init:true |> fun { run; close; _ } -> 
    assert(run (map_ops.find ~k:100) = None);
    assert(run (map_ops.find ~k:1000) = Some 1000);
    close ();
    ()
end
open Internal2

(** force the user to consider staging and fn when running the
   examples, by having a trivial functor *)
module Example() = struct

  (* actually execute the above *)
  let do_mini_check() = 
    do_write();
    do_delete();
    do_check();
    ()

  let do_full_check () = 
    print_endline "Full check...";
    btree_from_file ~fn:!fn ~create:true ~init:true |> fun { run; close; _ } -> 
    for k = 1 to max do
      if (100 <= k && k <= 200) then
        assert(run (map_ops.find ~k) = None)
      else
        assert(run (map_ops.find ~k) = Some(k))
    done;
    close ();
    ()

end
