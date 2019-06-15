(** Various examples *)
open Tjr_profile.Util.Profiler
open Tjr_btree
open Btree_intf

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
  blk_dev, blk_allocator), use {!Tjr_btree.disk_to_store} to
  construct a corresponding store
- then use {!Tjr_btree.store_to_map} to convert store to a map
  (using a root pointer to convert the pre_map_ops to a map_ops)

FIXME include this documentation in main tjr_btree lib, perhaps as a
simple int->int example

{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 

<img src="https://docs.google.com/drawings/d/e/2PACX-1vQXKtsYnp_Z4gUHTpYZOeLrGGIIQxPQrSSgdnoUylAW269ckYBMaUXz9MlDk8aHd1evYCSJNFGpqRFb/pub?w=960&amp;h=720">
%}

*)


(** {2 Abstract version...} 

We start with a version that abstracts over k,v, marshalling and blk_dev

*)

(** {2 Misc prelude} *)

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

module Misc = struct
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
end
open Misc


module Blk_ops = struct
  (* blocks etc *)
  let block_ops = String_block_ops.make_string_block_ops ~blk_sz 

  type blk_id = int
  type blk = string
end
open Blk_ops


module Blk_allocator = struct
  (** The first block that can be allocated; currently 2, since 0 is
     the root block, and 1 is the initial empty leaf (for an empty
     B-tree; may be mutated) *)
  let first_free_block = 2
  let blk_allocator_ref = alloc_fstore_ref {min_free_blk_id=first_free_block} 
  let blk_allocator : (blk_allocator_state,fstore_passing) with_state = 
    Fstore_passing.fstore_ref_to_with_state blk_allocator_ref
end
open Blk_allocator


module Btree_root_block = struct
  let initial_btree_root_block : blk_id = 1
  let btree_root_block_ref = alloc_fstore_ref initial_btree_root_block
  let root_ops = Fstore_passing.fstore_ref_to_with_state btree_root_block_ref
end
open Btree_root_block


module type BLK_DEV_OPS = sig
  val blk_dev_ops: (blk_id,blk,fstore_passing) blk_dev_ops
end


(** {2 Abstract version} *)


module type S = sig
  type k  (* we assume k_cmp = Pervasives.compare *)
  type v
  type r = blk_id
  type t = fstore_passing

  val monad_ops: t monad_ops

(*    
  type k_cmp
  val k_cmp : (k,k_cmp) Base.Map.comparator

  type kopt_cmp
  val kopt_cmp : (k option,kopt_cmp) Base.Map.comparator
*)    
(*
  val node_ops: (k,r,node)Isa_btree_intf.node_ops
  val leaf_ops: (k,v,leaf)Isa_btree_intf.leaf_ops
*)

  val read_k  : k Bin_prot.Type_class.reader
  val write_k : k Bin_prot.Type_class.writer
  val read_v  : v Bin_prot.Type_class.reader
  val write_v : v Bin_prot.Type_class.writer
  val k_size: int
  val v_size: int      
    
  include BLK_DEV_OPS
end



module Internal_abstract(S:S) = struct
  open S

  module Internal = struct 
    let k_cmp : k -> k -> int = Pervasives.compare

    let constants = 
      Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size 

    module S' = struct
        include S
        let k_cmp = k_cmp
        let cs = constants
      end
    module X = Tjr_btree.Make_disk_to_map(S')
    include X


    (* blocks etc *)
    let block_ops = Blk_ops.block_ops

    (* module Map_ops = Isa_export_wrapper.Internal_make_map_ops(struct type nonrec k=k let k_cmp=k_cmp end) *)
    (* node leaf conversions, for marshalling *)
    (* let node_ops = Leaf_node_frame_impls.make_node_ops ~map_ops:(kopt_map_ops) *)
    (* let leaf_ops = Leaf_node_frame_impls.make_leaf_ops ~map_ops:(k_map_ops) *)
                      
    (* marshalling *)
    let mp = 
      Bin_prot_marshalling.make_binprot_marshalling ~block_ops
        ~node_ops ~leaf_ops
        ~read_k ~write_k ~read_v ~write_v

    (* open Tjr_profile.Util.No_profiler *)

    let mp = Btree_intf.{
      dnode_to_blk=(fun dn -> profile "d2blk" @@ fun () -> mp.dnode_to_blk dn);
      blk_to_dnode=(fun blk -> profile "blk2d" @@ fun () -> mp.blk_to_dnode blk);
      marshal_blk_size=mp.marshal_blk_size;
    }

    let constants = 
      Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size 

    let print_constants () = 
      let cs = constants in
      if !Global.debug_global then 
        Printf.printf "Calculated constants: lmin,%d lmax,%d nmin,%d nmax,%d\n%!" 
          cs.min_leaf_size cs.max_leaf_size cs.min_node_keys cs.max_node_keys

    let blk_allocator_ops = 
      let { with_state } = blk_allocator in
      let alloc () = with_state (fun ~state:s ~set_state ->
          set_state {min_free_blk_id=s.min_free_blk_id+1} >>= fun _ 
          -> return s.min_free_blk_id)
      in
      let free _blk_id = return () in
      {alloc;free}

    let _ = blk_allocator_ops

    let disk_to_store = Tjr_btree.disk_to_store 

    let store_ops = 
      disk_to_store
        ~monad_ops 
        ~marshalling_ops:mp
        ~blk_dev_ops
        ~blk_allocator_ops

    let profile_m ~mark s m = 
      return () >>= fun () -> 
      mark s;
      m >>= fun r ->
      mark (s^"'");
      return r

    (* we add some profiling; we also take the opportunity to add some
       simple caching; FIXME add LRU caching for store *)
    let store_ops = 
      let {read;wrte;rewrite;free} = store_ops in
      (* Add some memoization *)
      let module L = Lru.M.Make(struct
          type t = blk_id
          let equal : t -> t -> bool = Pervasives.(=)  (* FIXME don't use pervasives for real code *)
          let hash: t -> int = Hashtbl.hash
        end)(struct
          type t = (node, leaf) dnode  (* FIXME dnode_impl *)
          let weight: t -> int = fun _ -> 1
        end)
      in    
      let cap = 52000 (* 51539*) in   (* FIXME config; (+ 10 ( * 227 227)) *)
      let slack = 10 in
      let lru = L.create cap in
      (* FIXME we perhaps want to avoid calling trim on every add
         (depending on the cost of performing a trim) *)
      let trim lru = 
        if L.size lru >= cap+slack then L.trim lru else ()
      in
      let read = fun r -> 
        L.find r lru |> function
        | None -> (read r >>= fun dn -> 
            L.add r dn lru; 
            trim lru;
            ();
            return dn)
        | Some dn -> 
          L.promote r lru;
          return dn
      in
      let wrte dn = 
        wrte dn >>= fun r -> 
        L.add r dn lru;
        trim lru;
        return r
      in
      let rewrite r dn = 
        rewrite r dn >>= function
        | None -> (
            (* updated in place *)
            L.add r dn lru;
            trim lru;
            return None)
        | Some r' -> (
          L.add r' dn lru;
          trim lru;
          return (Some r'))
      in
      {
        read=(fun r -> profile_m ~mark "ib" (read r));
        wrte=(fun dn -> profile_m ~mark "ic" (wrte dn));
        rewrite=(fun r dn -> profile_m ~mark "id" (rewrite r dn));
        free;
    }
  end
  open Internal

  module Internal2 = struct
    let empty_disk_leaf_as_blk = 
      let blk = lazy (
        leaf_ops.kvs_to_leaf [] |> fun x ->
        mp.dnode_to_blk (Disk_leaf x))
      in
      fun () -> Lazy.force blk

    let _ = empty_disk_leaf_as_blk

    let on_disk_util = 
      Map_on_fd_util.make ~block_ops ~empty_disk_leaf_as_blk 
  end

  (** from_file and close; Ignore this for in-mem versions *)
  let on_disk_util = Internal2.on_disk_util

  let pre_btree_ops = Isa_btree.

  (** Construct the map operations *)
  let map = 
    pre_btree_to_map
      ~monad_ops 
      ~cs:constants 
      ~k_args
      ~store_ops
      ~root_ops:{root_ops=root_ops}

  let _ :
k_args:('a -> 'a -> r,
        ('a, 'b, k_map) Isa_btree_intf.Leaf_node_frame_map_ops_type.map_ops,
        ('a option, r, kopt_map)
        Isa_btree_intf.Leaf_node_frame_map_ops_type.map_ops)
       Make_with_kargs.k_args ->
('a, 'b, fstore_passing) Map_ops_etc_type.map_ops_etc *
('a, 'b, r, 'xxx, fstore_passing) leaf_stream_ops
    = map
end


(** {2 Parameterize over blk_dev} 

Now we parameterize only over the blk dev
*)

module Internal_over_blk_dev(Blk_dev_ops:BLK_DEV_OPS) = struct
  open Blk_dev_ops

  module Int_int = struct
    module S = struct
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
      let blk_dev_ops = blk_dev_ops
    end
    include Internal_abstract(S)
    let _ = Internal.print_constants()
  end


  module Ss_ss = struct
    module S = struct
      open Bin_prot_marshalling
      type k = ss
      type v = ss
      let read_k = bin_reader_ss
      let write_k = bin_writer_ss
      let read_v = bin_reader_ss
      let write_v = bin_writer_ss
      let k_size = bp_size_ss
      let v_size = bp_size_ss
      type blk_id = int
      type blk = string
      let blk_dev_ops = blk_dev_ops
    end
    include Internal_abstract(S)
  end

  module Ss_int = struct
    module S = struct
      open Bin_prot_marshalling
      type k = ss
      type v = int
      let read_k = bin_reader_ss
      let write_k = bin_writer_ss
      let read_v = bin_reader_int
      let write_v = bin_writer_int
      let k_size = bp_size_ss
      let v_size = bp_size_int
      type blk_id = int
      type blk = string
      let blk_dev_ops = blk_dev_ops
    end
    include Internal_abstract(S)
  end
      
  let ii_map = Int_int.map
  let ss_map = Ss_ss.map
  let si_map = Ss_int.map
(*
[> `Map_ops of (int, int, fstore_passing) map_ops ] *
[> `Insert_many of unit ]
*)
end


(** {2 In-memory block dev} 

Finally, we can start instantiating.
*)

module In_mem_blk_dev : BLK_DEV_OPS = struct

  let blk_dev_ops = 
    let blk_dev_ref = alloc_fstore_ref (Tjr_map.With_pervasives_compare.empty ()) in
    let with_state = Tjr_fs_shared.Fstore_passing.fstore_ref_to_with_state blk_dev_ref in
    let _ = with_state in
    Blk_dev_in_mem.make 
      ~monad_ops 
      ~blk_sz:(bsz_of_int blk_sz)
      ~with_state

  let _ = blk_dev_ops
end

module In_mem = Internal_over_blk_dev(In_mem_blk_dev)

(** {2 On-disk block dev} *)

module On_disk_blk_dev (* : BLK_DEV_OPS *) = struct

  let blk_dev_ref = alloc_fstore_ref (None:Unix.file_descr option)

  let with_state = Fstore_passing.fstore_ref_to_with_state blk_dev_ref

  (* reuse the internal functionality *)
  open Blk_dev_on_fd.Internal

  let read_count = Global.register ~name:"Examples.read_count" (ref 0)
  let write_count = Global.register ~name:"Examples.write_count" (ref 0)
 
  (* open Tjr_profile.Util.No_profiler *)

  let read ~blk_id = with_state.with_state (fun ~state:(Some fd) ~set_state:_ -> 
      profile "fb" @@ fun () -> 
      incr(read_count);
      read ~block_ops ~fd ~blk_id |> return) [@@warning "-8"]

  let write ~blk_id ~blk = with_state.with_state (fun ~state:(Some fd) ~set_state:_ -> 
      profile "fc" @@ fun () -> 
      incr(write_count);
      write ~block_ops ~fd ~blk_id ~blk |> return) [@@warning "-8"]
 
  let blk_dev_ops = { blk_sz=(bsz_of_int blk_sz); read; write }
end

(** NOTE this requires that the fd is set in the initial fstore *)
module On_disk = Internal_over_blk_dev(On_disk_blk_dev)


(** NOTE disk utils can be accessed eg via {!On_disk.Int_int} (ignore
   the on_disk_util for in-mem versions) *)
