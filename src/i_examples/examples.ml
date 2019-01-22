(** Various examples *)

(* we work with a simple store-passing monad for the in-mem versions *)

(** {2 In-memory examples} *)

include struct
  open Tjr_fs_shared.Store_passing

  let make' ~blk_sz =

    let monad_ops = Tjr_fs_shared.Store_passing.monad_ops in

    let store = Tjr_store.initial_store in

    (* blocks etc *)
    let block_ops = 
      Tjr_fs_shared.Block_ops.String_block_ops.make_string_block_ops ~blk_sz in

    let store,blk_dev_in_mem =
      let store,r = 
        Tjr_store.mk_ref (Tjr_polymap.empty Pervasives.compare) store in
      let with_blk_dev_in_mem f = with_ref r f in
      let with_state = Tjr_monad.With_state.{ with_state=with_blk_dev_in_mem } in
      let blk_dev_in_mem = 
        Tjr_fs_shared.Blk_dev_in_mem.make ~monad_ops ~blk_sz ~with_state
      in
      store,blk_dev_in_mem
    in


    (* marshalling *)
    let open Bin_prot_util in
    let ii_mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops 
        ~read_k:bin_reader_int ~write_k:bin_writer_int
        ~read_v:bin_reader_int ~write_v:bin_writer_int
    in

    let ii_constants = 
      let open Bin_prot_util in
      Bin_prot_marshalling.make_constants 
        ~blk_sz ~k_size:bp_size_int ~v_size:bp_size_int
    in


    let ss_mp =
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops 
        ~read_k:bin_reader_ss ~write_k:bin_writer_ss
        ~read_v:bin_reader_ss ~write_v:bin_writer_ss
    in

    let ss_constants =
      let open Bin_prot_util in
      Bin_prot_marshalling.make_constants 
        ~blk_sz ~k_size:bp_size_ss ~v_size:bp_size_ss
    in


    let si_mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops 
        ~read_k:bin_reader_ss ~write_k:bin_writer_ss
        ~read_v:bin_reader_int ~write_v:bin_writer_int
    in

    let si_constants =
      let open Bin_prot_util in
      Bin_prot_marshalling.make_constants 
        ~blk_sz ~k_size:bp_size_ss ~v_size:bp_size_int
    in


    (* free space *)
    let first_free_block = 10 in
    let store,free_ops = 
      let store,r = Tjr_store.mk_ref first_free_block store in
      let with_free f = with_ref r f in
      let with_state = Tjr_monad.With_state.{ with_state=with_free } in
      let alloc () = with_state.with_state (fun ~state:s ~set_state ->
          set_state (s+1) >>= fun _ -> return s)
      in
      let free _blk_id = return () in
      store,Blk_allocator_ops_type.{alloc;free}
    in


    (* store *)
    let ii_mem_store = 
      Disk_to_store.disk_to_store 
        ~monad_ops 
        ~mp:ii_mp 
        ~blk_dev_ops:blk_dev_in_mem 
        ~free_ops 
    in

    let ss_mem_store = 
      Disk_to_store.disk_to_store 
        ~monad_ops 
        ~mp:ss_mp 
        ~blk_dev_ops:blk_dev_in_mem 
        ~free_ops 
    in

    let si_mem_store = 
      Disk_to_store.disk_to_store 
        ~monad_ops 
        ~mp:si_mp 
        ~blk_dev_ops:blk_dev_in_mem 
        ~free_ops 
    in

    (* map *)
    let ii_mem_map ~page_ref_ops = 
      Store_to_map.store_ops_to_map_ops
        ~monad_ops 
        ~constants:ii_constants 
        ~cmp:Pervasives.compare
        ~page_ref_ops
        ~store_ops:ii_mem_store
    in

    let ss_mem_map ~page_ref_ops =
      Store_to_map.store_ops_to_map_ops
        ~monad_ops 
        ~constants:ss_constants 
        ~cmp:Pervasives.compare
        ~page_ref_ops
        ~store_ops:ss_mem_store
    in

    (block_ops,store,ii_mp,ii_constants,ss_mp,ss_constants,si_mp,si_constants,ii_mem_map,ss_mem_map)
  [@@warning "-26-27"]


  let (block_ops,store,ii_mp,ii_constants,ss_mp,ss_constants,si_mp,si_constants,ii_mem_map,ss_mem_map) = make' ~blk_sz:4096
end
  


(** {2 On disk examples } *)

include struct

  open Map_on_fd_util

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  let free_ops = Blk_allocator_ops_type.{
    alloc=(fun () -> 
          free_ops.get () >>= fun x ->
          free_ops.set (x+1) >>= fun _ ->
          return x);
    free=(fun _ -> return ())
  }

  let mk_blk_dev_on_fd ~fd =
    Tjr_fs_shared.Blk_dev_on_fd.make_blk_dev_on_fd ~monad_ops ~block_ops ~fd

  let ii_fd_store = fun fd ->
    Disk_to_store.disk_to_store 
      ~monad_ops 
      ~mp:ii_mp 
      ~blk_dev_ops:(mk_blk_dev_on_fd ~fd)
      ~free_ops 

  let ss_fd_store = fun fd ->
    Disk_to_store.disk_to_store 
      ~monad_ops 
      ~mp:ss_mp 
      ~blk_dev_ops:(mk_blk_dev_on_fd ~fd)
      ~free_ops 

  let si_fd_store = fun fd ->
    Disk_to_store.disk_to_store 
      ~monad_ops 
      ~mp:si_mp 
      ~blk_dev_ops:(mk_blk_dev_on_fd ~fd)
      ~free_ops 


  let ii_fd_map ~page_ref_ops ~fd = 
    Store_to_map.store_ops_to_map_ops
      ~monad_ops 
      ~constants:ii_constants 
      ~cmp:Pervasives.compare
      ~page_ref_ops
      ~store_ops:(ii_fd_store fd)


  let ss_fd_map ~page_ref_ops ~fd = 
    Store_to_map.store_ops_to_map_ops
      ~monad_ops 
      ~constants:ss_constants 
      ~cmp:Pervasives.compare
      ~page_ref_ops
      ~store_ops:(ss_fd_store fd)


  let ii_map_on_fd  =
    let from_file ~fn ~create ~init =
      Map_on_fd_util.from_file ~block_ops ~mp:ii_mp ~fn ~create ~init in
    let close = Map_on_fd_util.close ~block_ops in
    let page_ref_ops = Map_on_fd_util.page_ref_ops in
    let rest ~ref_ =
      let map = ii_fd_map ~page_ref_ops ~fd:(!ref_).fd in
      let insert k v = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.insert k v)
      in
      let delete k = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.delete k)
      in
      let find k = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.find k)
      in
      let store_ops = ii_fd_store (!ref_).fd in
      let ls_ops = Store_to_map.store_ops_to_ls_ops 
          ~monad_ops ~constants:ii_constants ~cmp:Pervasives.compare ~store_ops
      in
      let mk_leaf_stream () = 
        let root = (!ref_).root in
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (ls_ops.mk_leaf_stream root)
      in
      let ls_step lss =
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (ls_ops.ls_step lss)
      in
      let ls_kvs lss = ls_ops.ls_kvs lss in
      (find,insert,delete,(mk_leaf_stream,ls_step,ls_kvs))
    in
    (from_file,close,rest)


  let ss_map_on_fd  =
    let from_file ~fn ~create ~init =
      Map_on_fd_util.from_file ~block_ops ~mp:ss_mp ~fn ~create ~init in
    let close = Map_on_fd_util.close ~block_ops in
    let page_ref_ops = Map_on_fd_util.page_ref_ops in
    let rest ~ref_ =
      let map = ss_fd_map ~page_ref_ops ~fd:(!ref_).fd in
      let insert k v = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.insert k v)
      in
      let delete k = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.delete k)
      in
      let find k = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.find k)
      in
      let store_ops = ss_fd_store (!ref_).fd in
      let ls_ops = Store_to_map.store_ops_to_ls_ops 
          ~monad_ops ~constants:ss_constants ~cmp:Pervasives.compare ~store_ops
      in
      let mk_leaf_stream () = 
        let root = (!ref_).root in
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (ls_ops.mk_leaf_stream root)
      in
      let ls_step lss =
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (ls_ops.ls_step lss)
      in
      let ls_kvs lss = ls_ops.ls_kvs lss in
      (find,insert,delete,(mk_leaf_stream,ls_step,ls_kvs))
    in
    (from_file,close,rest)


end
