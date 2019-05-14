(** A simple example of a kv store. *)

open Examples

let _ = 
  Printf.printf 
    "%s ----------------------------------------------\n%!" 
    __MODULE__

module Internal = struct
  let fn = "btree.store"


  (* let _ = Test.disable() *)
  let _ = Isa_btree.disable_isa_checks()

  let on_disk_util,(`Map_ops map_ops,_) = Examples.On_disk.Int_int.(on_disk_util,map)

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
    let set = Tjr_store.set in
    Tjr_store.initial_store 
    |> set ba_ref {min_free_blk_id=root_block.free}
    |> set rb_ref root_block.btree_root
    |> set bd_ref (Some fd)

  let close ~fd ~fstore =
    close ~fd 
      ~root_block:({free=(Tjr_store.get ba_ref !fstore).min_free_blk_id;
                    btree_root=Tjr_store.get rb_ref !fstore})
end
open Internal

module Internal2 = struct
  (* create and init store, write some values, and close *)
  let do_write () = 
    Printf.printf "Executing %d writes...\n%!" max;
    print_endline "Writing...";
    let fd,root_block = from_file ~fn ~create:true ~init:true in
    let fstore = ref (init_store ~fd ~root_block) in
    (* write values *)
    for x=1 to max do
      run ~fstore (map_ops.insert ~k:x ~v:x)
    done;
    close ~fd ~fstore;
    ()

  (* open store, delete some values, and close *)
  let do_delete () = 
    print_endline "Deleting...";
    let fd,root_block = from_file ~fn ~create:false ~init:false in
    let fstore = ref (init_store ~fd ~root_block) in
    for x=100 to 200 do
      run ~fstore (map_ops.delete ~k:x);
    done;
    close ~fd ~fstore;
    ()

  (* open store and check whether various keys and values are correct *)
  let do_check () = 
    print_endline "Checking...";
    let fd,root_block = from_file ~fn ~create:false ~init:false in
    let fstore = ref (init_store ~fd ~root_block) in
    assert(run ~fstore (map_ops.find ~k:100) = None);
    assert(run ~fstore (map_ops.find ~k:1000) = Some 1000);
    close ~fd ~fstore;
    ()
end
open Internal2

(* actually execute the above *)
let do_mini_check() = 
  do_write();
  do_delete();
  do_check();
  ()

let do_full_check () = 
  print_endline "Full check...";
  let fd,root_block = from_file ~fn ~create:false ~init:false in
  let fstore = ref (init_store ~fd ~root_block) in
  for k = 1 to max do
    if (100 <= k && k <= 200) then
      assert(run ~fstore (map_ops.find ~k) = None)
    else
      assert(run ~fstore (map_ops.find ~k) = Some(k))
  done;
  close ~fd ~fstore;
  ()

(* let _ = do_full_check () *)
