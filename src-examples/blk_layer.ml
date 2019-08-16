open Intf_
(* open Intf_.Staging *)

(** Help for maps on fd: write global state into root block; load back
   in from file *)

(* FIXME we already know what the empty disk leaf is, but the
   marshalling params allow us to convert to a blk; perhaps assume we
   already have empty_leaf_as_blk? *)

module Blk_init_and_close = struct
  type ('blk_id,'blk,'init,'fd,'t) blk_init_and_close = {
    blk_init: 'init -> ('fd,'t)m;
    blk_close: 'fd -> (unit,'t)m;
    fd_to_blk_dev: 'fd -> ('blk_id,'blk,'t)blk_dev_ops
  }
  let unix_init_and_close ~monad_ops ~blk_ops = {
    blk_init=(fun (fn,create,init) -> 
        let fd = Tjr_file.fd_from_file ~fn ~create ~init in
        monad_ops.return fd);
    blk_close=(fun fd -> 
        Unix.close fd; monad_ops.return ());
    fd_to_blk_dev=(fun fd -> Tjr_fs_shared.Blk_dev_on_fd.make_with_unix ~monad_ops ~blk_ops ~fd)
  }
end
open Blk_init_and_close
let unix_init_and_close = Blk_init_and_close.unix_init_and_close

module Internal_root_block_util = struct
      
  (** {2 Root blocks}

      We implement the map on fd by writing the free counter and root
      page_ref into the root block 

  *)

  (* TODO we use standard ocaml marshalling for the root block - this is
     a demo anyway *)
  type root_block = blk_id blk_allocator_state * blk_id btree_root_state

  let marshal_to_string (x:root_block) = Marshal.to_string x []

  let marshal_from_string s : root_block = Marshal.from_string s 0

  let root_blk_id = Blk_id.of_int 0

  let write_root_block ~blk_ops ~blk_dev_ops ~blk_allocator_state ~btree_root_state = 
    (blk_allocator_state,btree_root_state)
    |> marshal_to_string 
    |> blk_ops.of_string |> fun blk -> 
    blk_dev_ops.write ~blk_id:root_blk_id ~blk
  (* Blk_dev_on_fd.Internal_unix.write ~blk_ops ~fd () ~blk_id:root_blk_id ~blk *)

  let read_root_block ~monad_ops ~blk_ops ~(blk_dev_ops:('blk_id,'blk,'t)blk_dev_ops) = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    blk_dev_ops.read ~blk_id:root_blk_id >>= fun blk -> 
    (* Blk_dev_on_fd.Internal_unix.read ~blk_ops ~fd () ~blk_id:root_blk_id  *)
    blk |> blk_ops.to_string 
    |> (fun x -> (marshal_from_string x))
    |> return
end

let write_root_block,read_root_block = Internal_root_block_util.(write_root_block,read_root_block)

type ('a,'b) from_file_and_close = {
  from_file: 'a;
  close: 'b
}

let make_from_file_and_close ~monad_ops ~blk_ops ~empty_leaf_as_blk =
  let module A = struct
    (* let monad_ops = imperative_monad_ops *)
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return 

    (* NOTE this uses unix to read/write the root block; need to take
       care if also accessing fd from lwt *)
    let blk_init_and_close = unix_init_and_close ~monad_ops ~blk_ops
    let {blk_init; blk_close; fd_to_blk_dev}=blk_init_and_close

    (** NOTE empty_disk_leaf only needed for init *)
    let from_file ~fn ~create ~init = 
      blk_init (fn,create,init) >>= fun fd -> 
      let blk_dev_ops = fd_to_blk_dev fd in
      match init with
      | true -> (
          (* now need to write the initial dnode *)
          let blk = empty_leaf_as_blk in
          blk_dev_ops.write ~blk_id:(Blk_id.of_int 1) ~blk >>= fun () -> 
          (* 0,1 are taken so 2 is free; 1 is the root of the btree FIXME
             this needs to somehow match up with Examples.first_free_block
          *)
          let blk_allocator_state = {min_free_blk_id=Blk_id.of_int 2} in
          let btree_root_state = {btree_root=Blk_id.of_int 1} in
          (* remember to write blk0 *)
          write_root_block ~blk_ops ~blk_dev_ops ~blk_allocator_state ~btree_root_state >>= fun () ->
          return (fd,blk_allocator_state,btree_root_state))
      | false -> 
        read_root_block ~monad_ops ~blk_ops ~blk_dev_ops >>= fun (ba,bt) -> 
        return (fd,ba,bt)

    let _  = from_file

    let close ~fd ~blk_allocator_state ~btree_root_state = 
      let blk_dev_ops = fd_to_blk_dev fd in
      write_root_block ~blk_ops ~blk_dev_ops ~blk_allocator_state ~btree_root_state >>= fun () -> 
      blk_close fd
  end
  in 
  A.{from_file; close}


module Internal2 = struct
  open Btree_intf
  open Profilers_.Blk_profiler

  let [d2blk;d2blk';blk2d;blk2d';fb;fb';fc;fc'] = 
    ["d2blk";"d2blk'";"blk2d";"blk2d'";"fb";"fb'";"fc";"fc'"] |> List.map intern
  [@@ocaml.warning "-8"]

  (* FIXME this takes advantage of the fact that -1*m is m' *)
  let mark' s f = 
    mark s;
    f () |> fun r ->
    mark (-1*s);
    r

  let make_marshalling_ops ~blk_ops ~node_leaf_list_conversions
      ~reader_writers 
    =
    let mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~blk_ops
        ~node_leaf_list_conversions
        ~reader_writers
    in
    (* add some profiling *)
    let mp = {
      dnode_to_blk=(fun dn -> mark' d2blk (fun () -> mp.dnode_to_blk dn));
      blk_to_dnode=(fun blk -> mark' blk2d (fun () -> mp.blk_to_dnode blk));
      marshal_blk_size=mp.marshal_blk_size;
    }
    in
    mp

  let make_blk_allocator_ops ~monad_ops ~blk_allocator =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let { with_state } = blk_allocator in
    let alloc () = with_state (fun ~state:s ~set_state ->
        set_state {min_free_blk_id=Blk_id.incr s.min_free_blk_id} >>= fun _ 
        -> return s.min_free_blk_id)
    in
    let free _blk_id = return () in
    {alloc;free}

  let make_disk_ops ~marshalling_ops ~blk_dev_ops ~blk_allocator_ops =
    { marshalling_ops; blk_dev_ops; blk_allocator_ops }

  let _ = make_disk_ops
end

let make_blk_allocator_ops = Internal2.make_blk_allocator_ops

let make_marshalling_ops = Internal2.make_marshalling_ops

let make_disk_ops = Internal2.make_disk_ops


(* FIXME we probably want some unix and lwt instances here *)

(* see fs_shared 
  (** {2 On-disk block dev} *)

  module On_disk_blk_dev (* : BLK_DEV_OPS *) = struct
    open Monad_ops
    (* open Internal *)
    let blk_dev_ref = Fstore.on_disk_blk_dev_ref

    let with_state = Fstore_passing.fstore_ref_to_with_state blk_dev_ref

    (* reuse the internal functionality *)
    module Unix_ = Blk_dev_on_fd.Internal_unix

    let read_count = Global.register ~name:"Examples.read_count" (ref 0)
    let write_count = Global.register ~name:"Examples.write_count" (ref 0)

    (* NOTE in the following, we access the fd via the functional store *)
    let read ~blk_id = with_state.with_state (fun ~state:(Some fd) ~set_state:_ -> 
        mark' fb @@ fun () -> 
        incr(read_count);
        let blk_id = Blk_id.to_int blk_id in
        Unix_.read ~blk_ops ~fd () ~blk_id |> return) [@@warning "-8"]

    let _ = read

    let write ~blk_id ~blk = with_state.with_state (fun ~state:(Some fd) ~set_state:_ -> 
        mark' fc @@ fun () -> 
        incr(write_count);
        let blk_id = Blk_id.to_int blk_id in      
        Unix_.write ~blk_ops ~fd () ~blk_id ~blk |> return) [@@warning "-8"]

    let blk_dev_ops = { blk_sz=Blk_sz.blk_sz_4096; read; write }
  end
  let on_disk_blk_dev = On_disk_blk_dev.blk_dev_ops
*)


(*
  (** Use the on-disk blk dev to construct various instances *)
  module C = Bin_prot_marshalling.Common_reader_writers

  module Common_blk_layers = struct
    module Int_int = struct
      let blk_dev_ops = on_disk_blk_dev

      let reader_writers = C.int_int

      let disk_ops ~node_leaf_list_conversions = make_disk_ops ~blk_dev_ops ~reader_writers ~node_leaf_list_conversions
    end

    module Ss_ss = struct
      let blk_dev_ops = on_disk_blk_dev

      let reader_writers = C.ss_ss

      let disk_ops ~node_leaf_list_conversions = make_disk_ops ~blk_dev_ops ~reader_writers ~node_leaf_list_conversions
    end


    module Ss_int = struct
      let blk_dev_ops = on_disk_blk_dev

      let reader_writers = C.ss_int

      let disk_ops ~node_leaf_list_conversions = make_disk_ops ~blk_dev_ops ~reader_writers ~node_leaf_list_conversions
    end
  end
end

let make_disk_ops,in_mem_blk_dev,on_disk_blk_dev = 
  Internal.(make_disk_ops,in_mem_blk_dev,on_disk_blk_dev)

module Common_blk_layers = Internal.Common_blk_layers



(*
  (** Additional functionality for fstore FIXME remove *)
  module Fstore_util = struct
    (** {2 Root block initialization/finalization... Init and close} *)
    open Fstore_layer.Fstore

    (** Given various parameters, this sets up the initial tjr_store,
       so that the various refs agree with the parameters. *)
    let init_tjr_store ~fd ~(root_block:root_block) =
      (* Printf.printf "Init with free=%d and root=%d\n%!" root_block.free root_block.btree_root; *)
      let set = Tjr_store.set in
      !Fstore_layer.Fstore.R.fstore
      |> set blk_allocator_ref {min_free_blk_id=(Blk_id.to_int root_block.free)}
      |> set btree_root_block_ref root_block.btree_root
      |> set on_disk_blk_dev_ref (Some fd)

    let _ = init_tjr_store

    let close ~fd ~fstore =
      let free = (Tjr_store.get blk_allocator_ref !fstore).min_free_blk_id |> Blk_id.of_int in
      let btree_root = Tjr_store.get btree_root_block_ref !fstore in
      (* Printf.printf "Close with free=%d and root=%d\n%!" free btree_root; *)
      close ~fd ~root_block:{free; btree_root}

    let btree_from_file ~fn ~create ~init =
      let fd,root_block = from_file ~fn ~create ~init in
      let fstore = ref (init_tjr_store ~fd ~root_block) in
      let run x = run ~fstore x in
      let close () = close ~fd ~fstore in
      { fd; root_block; fstore; run; close }

    let _ = btree_from_file

  end
  in
  { btree_from_file=A.btree_from_file }

(** Specialize to Fstore_layer.blk_ops *)
let make_btree_from_file ~empty_leaf_as_blk = 
  make_btree_from_file ~blk_ops:Fstore_layer.blk_ops ~empty_leaf_as_blk

let _ = make_btree_from_file
*)



(* see Tjr_fs_shared.Blk_dev_in_mem
  (** {2 In-memory block dev} *)

  module In_mem_blk_dev = struct
    let blk_dev_ref = Fstore.in_mem_blk_dev_ref
    let blk_dev_ops = 
      let with_state = Tjr_fs_shared.Fstore_passing.fstore_ref_to_with_state blk_dev_ref in
      let _ = with_state in
      Blk_dev_in_mem.make_blk_dev_in_mem
        ~monad_ops 
        ~blk_sz
        ~with_state

    let _ = blk_dev_ops
  end
  let in_mem_blk_dev = In_mem_blk_dev.blk_dev_ops
*)
*)
