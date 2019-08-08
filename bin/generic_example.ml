(** A simple example of a kv store. *)

type t = {
  do_write : unit -> unit;
  do_delete : unit -> unit;
  do_check : unit -> unit;
  do_full_check : unit -> unit;
  do_all : unit -> unit
}

let profile s f = Tjr_profile.measure_execution_time_and_print s f

type 't run = {
  run: 'a. ('a,'t)m -> 'a
}

let make_generic_example (type k v r leaf_stream t) 
    ~(map_ops_with_ls: (k,v,r,leaf_stream,t)Btree_intf.Map_ops_with_ls.map_ops_with_ls)
    ~run
    ~int_to_k ~int_to_v
  =

  let module A = struct

    (* let {btree_from_file} = btree_from_file  *)
    
    (* FIXME config *)
    let max_writes = 10000

    let {run} = run

    let ops = map_ops_with_ls

    (* create and init store, write some values, and close *)
    let do_write () = profile "write" @@ fun () -> 
      Printf.printf "Executing %d writes...\n%!" max_writes;
      print_endline "Writing...";
      (* write values *)
      for x=1 to max_writes do
        let k,v = int_to_k x,int_to_v x in
        run (ops.insert ~k ~v)
      done

    let _ = do_write

    (* delete some values *)
    let do_delete () = profile "del" @@ fun () -> 
      print_endline "Deleting...";
      for x=100 to 200 do
        let k = int_to_k x in
        run (ops.delete ~k);
      done

    (* open store and check whether various keys and values are correct *)
    let do_check () = 
      print_endline "Checking...";
      assert(run (ops.find ~k:(int_to_k 100)) = None);
      assert(run (ops.find ~k:(int_to_k 1000)) = Some (int_to_v 1000));
      ()
      
    let do_full_check () = profile "full" @@ fun () -> 
      print_endline "Full check...";
      for i = 1 to max_writes do
        let k = int_to_k i in
        if (100 <= i && i <= 200) then
          assert(run (ops.find ~k) = None)
        else
          assert(run (ops.find ~k) = Some(int_to_v i))
      done

    (* actually execute the above *)
    let do_all() = 
      do_write ();
      do_delete ();
      do_check ();
      do_full_check()      
  end
  in
  A.{  
    do_write;
    do_delete;
    do_check;
    do_full_check;
    do_all}


let _ = make_generic_example


let make_generic_main ~fn ~int_to_k ~int_to_v ~example = 
  let Examples.{monad_ops;blk_ops;empty_leaf_as_blk; blk_allocator_ref; btree_root_ref; _} = example in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let Blk_layer.{ from_file; close } =
    Blk_layer.make_from_file_and_close ~monad_ops ~blk_ops
    ~empty_leaf_as_blk in
  from_file ~fn ~create:true ~init:true >>= fun (fd,ba_root,bt_root) -> 
  let run = {run=Tjr_monad.Imperative.of_m} in
  btree_root_ref := bt_root;
  blk_allocator_ref := ba_root; 
  let map_ops_with_ls = example.map_ops_with_ls fd in
  let example = make_generic_example
      ~map_ops_with_ls
      ~run 
      ~int_to_k ~int_to_v
  in
  example.do_all ();
  close ~fd ~blk_allocator_state:!blk_allocator_ref ~btree_root_state:!btree_root_ref
