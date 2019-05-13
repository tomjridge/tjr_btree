(** Various examples *)
(* open Isa_btree *)
open Tjr_btree
(* open Bin_prot_marshalling *)

(* we work with a simple store-passing monad for the in-mem versions *)

(** The steps to construct an example are:

- fix the monad type (eg store passing)
- construct block ops {!Tjr_fs_shared.Block_ops}, which converts
  strings/bytes to/from blks
- construct block device ops {!Tjr_fs_shared.Blk_dev_ops_type}, which
  reads and writes to blks
- implement {!Bin_prot_marshalling.node_leaf_conversions} to convert
  from list-based leaf/node impls to the efficient leaf/node impls
- implement marshalling procedures via {!Bin_prot_marshalling}
- calculate constants based on blk_sz and key and value types, and
  marshalling strategy
- implement blk_allocator_ops, to allow allocation of blks via id
- for every desired combination of (key/value types, marshalling,
  blk_dev, blk_allocator), use {!Tjr_btree.uncached_disk_to_store} to
  construct a corresponding store
- then use {!Tjr_btree.store_ops_to_map_ops} to convert store to a map
  (using a root pointer to convert the pre_map_ops to a map_ops)

FIXME include this documentation in main tjr_btree lib, perhaps as a
simple int->int example

{%html: <img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> %}

*)


(** {2 Abstract version} 

We start with a version that abstracts over k,v, marshalling and blk_dev

*)

(* common to all impls *)
type blk_allocator_state = {
  min_free_blk_id:int;
}

(* FIXME free space; note the blk_allocator itself has to leave room for eg the root block *)

(** The global state is represented by a store; within the store
    there are refs to the block device, the block allocator state,
    ... 

    - blk_dev state
    - blk_allocator state

*)

let blk_sz = 4096

let monad_ops = fstore_passing_monad_ops
let ( >>= ) = monad_ops.bind
let return = monad_ops.return

(** A store, which we mutably change while initializing *)
let fstore = ref Tjr_store.initial_store

let alloc_fstore_ref = 
  fun x -> 
    Tjr_store.mk_ref x !fstore |> fun (store',r) ->
    fstore:=store';
    r

let _ = alloc_fstore_ref

module Blk_allocator = struct
  let blk_allocator : (blk_allocator_state,fstore_passing) with_state = 
    let r = alloc_fstore_ref {min_free_blk_id=2} in  (* FIXME 2??? *)
    Fstore_passing.fstore_ref_to_with_state r
end
open Blk_allocator


module type S =  sig
  type k  (* we assume k_cmp = Pervasives.compare *)
  type v
  (* type r *)

  val read_k  : k Bin_prot.Type_class.reader
  val write_k : k Bin_prot.Type_class.writer
  val read_v  : v Bin_prot.Type_class.reader
  val write_v : v Bin_prot.Type_class.writer
  val k_size: int
  val v_size: int      

  type blk_id = int
  type blk = string
  val blk_dev_ops: (blk_id,blk,fstore_passing) blk_dev_ops
end


module Internal_abstract(S:S) = struct
  open S

  let k_cmp = Pervasives.compare

  (* blocks etc *)
  let block_ops = String_block_ops.make_string_block_ops ~blk_sz 

  (* node leaf conversions, for marshalling *)
  let nlc () = Isa_btree.Isa_export_wrapper.node_leaf_conversions ~k_cmp

  (* marshalling *)
  let mp = 
    Bin_prot_marshalling.make_binprot_marshalling ~block_ops
      ~node_leaf_conversions:(nlc()) ~read_k ~write_k ~read_v ~write_v

  let constants = Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size

  let blk_allocator_ops = 
    let { with_state } = blk_allocator in
    let alloc () = with_state (fun ~state:s ~set_state ->
      set_state {min_free_blk_id=s.min_free_blk_id+1} >>= fun _ 
      -> return s.min_free_blk_id)
    in
    let free _blk_id = return () in
    {alloc;free}

  let _ = blk_allocator_ops

  let disk_to_store = Tjr_btree.uncached_disk_to_store 

  let store_ops = 
    disk_to_store
      ~monad_ops 
      ~marshalling_ops:mp
      ~blk_dev_ops
      ~blk_allocator_ops

  (* map *)
  let map ~root_ops = 
    store_ops_to_map_ops
      ~monad_ops 
      ~cs:constants 
      ~k_cmp
      ~root_ops
      ~store_ops

  let _ : root_ops:(blk_id, fstore_passing) root_ops ->
    [> `Map_ops of
         (k, v, fstore_passing) map_ops ] *
      [> `Insert_many of unit ] = map
end



module In_memory = struct

  let blk_dev_ops () = 
    let blk_dev_ref = alloc_fstore_ref (Tjr_poly_map.empty) in
    let with_state = {
      with_state=fun _f -> failwith "FIXME need to construct with_state easily from a blk_ref"
    }
    in
    Blk_dev_in_mem.make 
      ~monad_ops 
      ~blk_sz:(bsz_of_int blk_sz)
      ~with_state

  let _ :unit -> ('a, 'b, Tjr_store.t state_passing) blk_dev_ops
    = blk_dev_ops

  module Int_int' = struct
    open Bin_prot_marshalling
    type k = int
    type v = int
    let read_k = bin_reader_int
    let write_k = bin_writer_int
    let read_v = bin_reader_int
    let write_v = bin_writer_int
    let k_size = bp_size_int
    let v_size = bp_size_int
    type blk_id = int
    type blk = string
    let blk_dev_ops = blk_dev_ops()      
  end
(* 
sig
  val k_cmp : 'a -> 'a -> int
  val block_ops : string block_ops
  val nlc :
    unit ->
    ('a, 'b, 'c, ('a, 'c) node_impl, ('a, 'b) leaf_impl)
    node_leaf_conversions
  val mp :
    (((int, int) node_impl, (int, int) leaf_impl) dnode, string)
    marshalling_ops
  val constants : constants
  val blk_allocator_ops : (int, fstore_passing) blk_allocator_ops
  val disk_to_store :
    monad_ops:'a monad_ops ->
    marshalling_ops:('b, 'c) marshalling_ops ->
    blk_dev_ops:('d, 'c, 'a) blk_dev_ops ->
    blk_allocator_ops:('d, 'a) blk_allocator_ops -> ('d, 'b, 'a) store_ops
  val store_ops :
    (int, ((int, int) node_impl, (int, int) leaf_impl) dnode, fstore_passing)
    store_ops
  val map :
    root_ops:(int, fstore_passing) with_state ->
    [> `Map_ops of (int, int, fstore_passing) map_ops ] *
    [> `Insert_many of unit ]
end
*)
  module Int_int = Internal_abstract(Int_int')

  module X = Int_int  (* FIXME this type is too general *)
      
  let ii_map = Int_int.map
(*
root_ops:(int, fstore_passing) with_state ->
[> `Map_ops of (int, int, fstore_passing) map_ops ] *
[> `Insert_many of unit ]
*)


end

(*
(** {2 In-memory examples} *)

module Internal_in_mem = struct
  open Tjr_fs_shared.Store_passing

  let make' ~blk_sz =

    let monad_ops = Tjr_fs_shared.Store_passing.monad_ops in

    let store = Tjr_store.initial_store in

    (* blocks etc *)
    let block_ops = String_block_ops.make_string_block_ops ~blk_sz in

    let store,blk_dev_in_mem =
      let store,r = 
        Tjr_store.mk_ref (Tjr_poly_map.empty ()) store in
      let with_blk_dev_in_mem f = with_ref r f in
      let with_state = Tjr_monad.With_state.{ with_state=with_blk_dev_in_mem } in
      let blk_dev_in_mem = 
        Tjr_fs_shared.Blk_dev_in_mem.make ~monad_ops ~blk_sz ~with_state
      in
      store,blk_dev_in_mem
    in

    let _ = store in

    (* node leaf conversions, for marshalling *)
    let nlc () = Isa_btree.Isa_export_wrapper.node_leaf_conversions ~k_cmp:Pervasives.compare in

    (* marshalling *)
    let ii_mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops ~node_leaf_conversions:(nlc())
        ~read_k:bin_reader_int ~write_k:bin_writer_int
        ~read_v:bin_reader_int ~write_v:bin_writer_int
    in

    let ii_constants = 
      Bin_prot_marshalling.make_constants 
        ~blk_sz ~k_size:bp_size_int ~v_size:bp_size_int
    in


    let ss_mp =
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops ~node_leaf_conversions:(nlc())
        ~read_k:bin_reader_ss ~write_k:bin_writer_ss
        ~read_v:bin_reader_ss ~write_v:bin_writer_ss
    in

    let ss_constants =
      Bin_prot_marshalling.make_constants 
        ~blk_sz ~k_size:bp_size_ss ~v_size:bp_size_ss
    in


    let si_mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops ~node_leaf_conversions:(nlc())
        ~read_k:bin_reader_ss ~write_k:bin_writer_ss
        ~read_v:bin_reader_int ~write_v:bin_writer_int
    in

    let si_constants =
      Bin_prot_marshalling.make_constants 
        ~blk_sz ~k_size:bp_size_ss ~v_size:bp_size_int
    in


    (* free space *)
    let first_free_block = 10 in
    let store,blk_allocator_ops = 
      let store,r = Tjr_store.mk_ref first_free_block store in
      let with_free f = with_ref r f in
      let with_state = Tjr_monad.With_state.{ with_state=with_free } in
      let alloc () = with_state.with_state (fun ~state:s ~set_state ->
          set_state (s+1) >>= fun _ -> return s)
      in
      let free _blk_id = return () in
      store,{alloc;free}
    in

    
    (* store *)
    let disk_to_store = Tjr_btree.uncached_disk_to_store in

    let ii_mem_store = 
      disk_to_store
        ~monad_ops 
        ~marshalling_ops:ii_mp 
        ~blk_dev_ops:blk_dev_in_mem 
        ~blk_allocator_ops
    in

    let _ = ii_mem_store in

    let ss_mem_store = 
      disk_to_store 
        ~monad_ops 
        ~marshalling_ops:ss_mp 
        ~blk_dev_ops:blk_dev_in_mem 
        ~blk_allocator_ops 
    in

    let si_mem_store = 
      disk_to_store 
        ~monad_ops 
        ~marshalling_ops:si_mp 
        ~blk_dev_ops:blk_dev_in_mem 
        ~blk_allocator_ops 
    in

    (* map *)
    let ii_mem_map ~root_ops = 
      store_ops_to_map_ops
        ~monad_ops 
        ~cs:ii_constants 
        ~k_cmp:Pervasives.compare
        ~root_ops
        ~store_ops:ii_mem_store
    in

    let _ = ii_mem_map in

    let ss_mem_map ~root_ops =
      store_ops_to_map_ops
        ~monad_ops 
        ~cs:ss_constants 
        ~k_cmp:Pervasives.compare
        ~root_ops
        ~store_ops:ss_mem_store
    in

    (block_ops,store,ii_mp,ii_constants,ss_mp,ss_constants,si_mp,si_constants,ii_mem_map,ss_mem_map)
  [@@warning "-26-27"]


  let (block_ops,store,ii_mp,ii_constants,ss_mp,ss_constants,si_mp,si_constants,ii_mem_map,ss_mem_map) = make' ~blk_sz:4096
end  (* Internal_in_mem *)

let ii_mem_map,ss_mem_map = Internal_in_mem.(ii_mem_map,ss_mem_map)
  

(** {2 On disk examples } *)

(*
module Internal_on_disk = struct

  open Internal_in_mem

  module Internal = struct

    (** Simple record to record results of various examples *)
    type ('a,'b,'c) t1 = {
      from_file:'a;
      close:'b;
      rest:'c
    }

    type ('a,'b,'c) t2 = {
      map_ops:'a;
      leaf_stream_ops:'b;
      imperative_ops:'c;
    }

    type ('a,'b,'c,'d,'e) t3 = {
      find:'a;
      insert:'b;
      delete:'c;
      insert_many:'d;
      insert_all:'e
    }
  end
  open Internal

  open Map_on_fd_util

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  let blk_allocator_ops = Tjr_btree.{
    alloc=(fun () -> 
          free_ops.get () >>= fun x ->
          free_ops.set (x+1) >>= fun _ ->
          return x);
    free=(fun _ -> return ())
  }

  let mk_blk_dev_on_fd ~fd =
    Tjr_fs_shared.Blk_dev_on_fd.make_blk_dev_on_fd ~monad_ops ~block_ops ~fd

  let disk_to_store = Tjr_btree.uncached_disk_to_store

  let ii_fd_store = fun fd ->
    disk_to_store 
      ~monad_ops 
      ~marshalling_ops:ii_mp 
      ~blk_dev_ops:(mk_blk_dev_on_fd ~fd)
      ~blk_allocator_ops 

  let ss_fd_store = fun fd ->
    disk_to_store 
      ~monad_ops 
      ~marshalling_ops:ss_mp 
      ~blk_dev_ops:(mk_blk_dev_on_fd ~fd)
      ~blk_allocator_ops 

  let si_fd_store = fun fd ->
    disk_to_store 
      ~monad_ops 
      ~marshalling_ops:si_mp 
      ~blk_dev_ops:(mk_blk_dev_on_fd ~fd)
      ~blk_allocator_ops 


  let ii_fd_map fd = 
    store_ops_to_map_ops
      ~monad_ops 
      ~cs:ii_constants 
      ~k_cmp:Pervasives.compare
      ~root_ops
      ~store_ops:(ii_fd_store fd)


  let ss_fd_map fd = 
    store_ops_to_map_ops
      ~monad_ops 
      ~cs:ss_constants 
      ~k_cmp:Pervasives.compare
      ~root_ops
      ~store_ops:(ss_fd_store fd)


  let ii_map_on_fd  =
    let from_file ~fn ~create ~init =
      Map_on_fd_util.from_file ~block_ops ~mp:ii_mp ~fn ~create ~init in
    let close = Map_on_fd_util.close ~block_ops in
    let rest ~ref_ =
      let (`Map_ops map,`Insert_many _insert_many) = ii_fd_map (!ref_).fd in
      let find k = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.find ~k)
      in
      (* at this point we want to substitute insert with a version that mutates blocks on disk *)
      (*
      let insert = Isa_export_wrapper.(
          let blk_dev_ops = mk_blk_dev_on_fd ~fd:(!ref_).fd in
          insert
            ~monad_ops
            ~cs:(ii_constants.min_leaf_size, ii_constants.max_leaf_size, ii_constants.min_node_keys, ii_constants.max_node_keys)
            ~k_cmp:(Pervasives.compare)
            ~store_ops:(
              let f (x:('k,'v,'r)Frame.frame) = match x with
                | Disk_node (ks,rs) ->
                  Isa_btree.Insert_with_mutation.Pre_monad.Disk_node(ks,rs)
                | Disk_leaf kvs -> 
                  (* FIXME add these constructors to the wrapper *)
                  Isa_btree.Insert_with_mutation.Pre_monad.Disk_leaf(kvs)
              in
              let g (x:('k,'v,'r)Isa_btree.Insert_with_mutation.Pre_monad.dnode) = match x with
                | Disk_node(ks,rs) -> Frame.Disk_node(ks,rs)
                | Disk_leaf (kvs) -> Frame.Disk_leaf(kvs)
              in
              let read r = 
                blk_dev_ops.read ~blk_id:r >>= fun blk -> 
                ii_mp.page_to_frame blk |> f |> return
              in
              let wrte dnode = 
                free_ops.alloc () >>= fun blk_id ->
                dnode |> g |> ii_mp.frame_to_page |> fun blk ->
                blk_dev_ops.write ~blk_id ~blk >>= fun () ->
                return blk_id
              in
              let rewrite r dnode = 
                dnode |> g |> ii_mp.frame_to_page |> fun blk ->
                blk_dev_ops.write ~blk_id:r ~blk >>= fun () ->
                return None  (* always overwrite *)
              in
              (read,wrte,rewrite)))
      in
*)
      (* but this insert uses explicit r passing; we want to go via page_ref *)
(*      let insert k v = 
        page_ref_ops.get () >>= fun r ->
        insert ~r ~k ~v >>= fun r' ->
        match r' with 
        | None -> return ()
        | Some r' -> page_ref_ops.set r'
      in*)
      (* NOTE the following now uses the insert from above *)
      let insert k v = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.insert ~k ~v)
      in
      let delete k = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.delete ~k)
      in
      let insert_many k v kvs = 
        failwith "FIXME implement"
(*
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.insert_many k v kvs)*)
      in        
      let insert_all _kvs = 
        failwith "FIXME implement"
(*
        match kvs with 
        | [] -> ()
        | (k,v)::kvs -> (
            Tjr_monad.State_passing.convert_to_imperative
              ref_        
              (Leaf_stream_util.insert_all ~monad_ops map.insert_many k v kvs)) *)
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
      { map_ops=map;
        leaf_stream_ops=(mk_leaf_stream, ls_step, ls_kvs);
        imperative_ops={find; insert; delete; insert_many; insert_all}}
    in
    { from_file; close; rest }

(*
  let ss_map_on_fd  =
    let from_file ~fn ~create ~init =
      Map_on_fd_util.from_file ~block_ops ~marshalling_ops:ss_mp ~fn ~create ~init in
    let close = Map_on_fd_util.close ~block_ops in
    let page_ref_ops = Map_on_fd_util.page_ref_ops in
    let rest ~ref_ =
      let map = ss_fd_map ~page_ref_ops ~fd:(!ref_).fd in
      let find k = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.find k)
      in
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
      let insert_many k v kvs = 
        Tjr_monad.State_passing.convert_to_imperative
          ref_
          (map.insert_many k v kvs)
      in        
      let insert_all kvs = 
        match kvs with 
        | [] -> ()
        | (k,v)::kvs -> (
            Tjr_monad.State_passing.convert_to_imperative
              ref_        
              (Leaf_stream_util.insert_all ~monad_ops map.insert_many k v kvs))
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
      { map_ops = map;
        leaf_stream_ops=(mk_leaf_stream, ls_step, ls_kvs);
        imperative_ops={find; insert; delete; insert_many; insert_all}}
    in
    { from_file; close; rest }
*)
end
*)
*)
