open Tjr_btree
open Block
open Frame
open Tree
open Rstk
open Monad
open Pre_map_ops
open Store_ops

module type Blk_types = sig type blk type blk_id = int end

module Constants_type :
  sig
    type t =
      Constants.Constants_type.t = {
      min_leaf_size : int;
      max_leaf_size : int;
      min_node_keys : int;
      max_node_keys : int;
    } [@@deriving yojson]
  end

module Frame_type :
  sig
    type ('k, 'v, 'r) frame =
      ('k, 'v, 'r) Isa_export.Disk_node.dnode =
        Disk_node of ('k list * 'r list)
      | Disk_leaf of ('k * 'v) list [@@deriving yojson]
  end

module Ls_state :
  sig
    type ('k, 'v, 'r) ls_state =
        ('k, 'v, 'r) Isa_export.Leaf_stream.ls_state
  end

module Params :
  sig
  end

module R2f :
  sig
    type ('k, 'v, 'r, 't) r2f = 't -> 'r -> ('k, 'v, 'r) frame option
  end

module R2t :
  sig
    type ('k, 'v, 'r, 't) r2t = 't -> 'r -> ('k, 'v) tree option
  end

module Rstk :
  sig
    type ('k, 'r) rstk = ('k, 'r) Isa_export.Tree_stack.rstack
  end

module Tree_type :
  sig
    type ('a, 'b) tree =
      ('a, 'b) Tree.tree =
        Node of ('a list * ('a, 'b) tree list)
      | Leaf of ('a * 'b) list  [@@deriving yojson]
  end

module Disk_ops :
  sig
    module Block_device_type = Disk_ops.Block_device_type
    type 't block_device =
      't Block_device_type.block_device = {
      blk_sz : blk_sz;
      read : blk_id -> (blk, 't) m;
      write : blk_id -> blk -> (unit, 't) m;
    }
  end

module Leaf_stream_ops :
  sig
    module Leaf_stream_types = Leaf_stream_ops.Leaf_stream_types
    type ('k, 'v, 'r) leaf_stream_state =
      ('k, 'v, 'r) Leaf_stream_types.leaf_stream_state = {
      kvs : ('k * 'v) list;
      ls : ('k, 'v, 'r) Ls_state.ls_state;
    }
    type ('k, 'v, 'r) lss = ('k, 'v, 'r) leaf_stream_state
    type ('k, 'v, 'r, 't) leaf_stream_ops =
      ('k, 'v, 'r, 't) Leaf_stream_types.leaf_stream_ops = {
      mk_leaf_stream : unit -> (('k, 'v, 'r) lss, 't) m;
      ls_step : ('k, 'v, 'r) lss -> (('k, 'v, 'r) lss option, 't) m;
      ls_kvs : ('k, 'v, 'r) lss -> ('k * 'v) list;
    }
  end

module Map_ops :
  sig
    module Map_ops_type = Map_ops.Map_ops_type
    type ('k, 'v, 't) map_ops =
      ('k, 'v, 't) Map_ops_type.map_ops = {
      find : 'k -> ('v option, 't) m;
      insert : 'k -> 'v -> (unit, 't) m;
      delete : 'k -> (unit, 't) m;
      insert_many :
        'k -> 'v -> ('k * 'v) list -> (('k * 'v) list, 't) m;
    }
  end

module Pre_map_ops :
  sig
    module Pre_map_ops_type = Pre_map_ops.Pre_map_ops_type
    type ('k, 'v, 'r, 't) pre_map_ops =
      ('k, 'v, 'r, 't) Pre_map_ops_type.pre_map_ops = {
      find_leaf : 'k -> 'r ->
        ('r * ('k * 'v) list * ('k, 'r) rstk, 't) m;
      find : 'k -> 'r -> ('r * ('k * 'v) list, 't) m;
      insert : 'k -> 'v -> 'r -> ('r, 't) m;
      insert_many : 
        'k -> 'v -> ('k * 'v) list -> 
        'r -> ('r * ('k * 'v) list, 't) m;
      delete : 'k -> 'r -> ('r, 't) m;
    }
  end

module Store_ops :
  sig
    type ('k, 'v, 'r, 't) store_ops =
      ('k, 'v, 'r, 't) Store_ops.store_ops = {
      store_free : 'r list -> (unit, 't) m;
      store_read : 'r -> (('k, 'v, 'r) frame, 't) m;
      store_alloc : ('k, 'v, 'r) frame -> ('r, 't) m;
    }
  end

module Small_step_types :
  sig
    type ('k, 'v, 'r) find_state =
        ('k, 'v, 'r) Isa_export.Find.find_state
    type ('k, 'v, 'r) insert_state =
        ('k, 'v, 'r) Isa_export.Insert.insert_state
    type ('k, 'v, 'r) im_state =
        ('k, 'v, 'r) Isa_export.Insert_many.ist
    type ('k, 'v, 'r) delete_state =
        ('k, 'v, 'r) Isa_export.Delete2.delete_state
  end

type page_ref_int = Page_ref_int.page_ref

module Store_to_map :
  sig
    type 't page_ref_ops =
        (page_ref_int, 't) mref
    val store_ops_to_map_ops :
      ps:< cmp : 'k -> 'k -> int; constants : Constants.t; .. > ->
      page_ref_ops:('a, 't) mref ->
      store_ops:('k, 'v, 'a, 't) store_ops -> 
      ('k, 'v, 't) Map_ops.map_ops
  end

module Disk_on_fd :
  sig
    type fd = Unix.file_descr
    type 't fd_ops = (fd, 't) mref
    val read :
      fd:Unix.file_descr ->
      blk_sz:blk_sz -> 
      blk_id:int -> blk
    val write :
      fd:Unix.file_descr ->
      blk_sz:int -> 
      blk_id:int -> 
      blk:blk -> unit
    val safely_ : string -> ('a, 't) m -> ('a, 't) m
    val make_disk :
      blk_sz:blk_sz ->
      fd_ops:(Unix.file_descr, 't) mref ->
      't Disk_ops.block_device
  end

module Disk_to_store :
  sig
    type 't free_ops = (int, 't) mref
    val disk_to_store :
      ps:< blk_sz : int;
           frame_to_page : int -> ('k, 'v, blk_id) frame -> blk;
           page_to_frame : blk -> ('k, 'v, page_ref_int) frame;
           .. > ->
      disk_ops:'t Disk_ops.block_device ->
      free_ops:(blk_id, 't) mref -> 
      ('k, 'v, blk_id, 't) store_ops
  end

module Mem_store :
  sig
    type ('k, 'v) mem =
      ('k, 'v) Mem_store.mem = {
      free : int;
      map : ('k, 'v) Page_ref_int.frame Map_int.t;
    }
    type ('k, 'v, 't) mem_ops = (('k, 'v) mem, 't) mref
  end

module Cache :
  sig
    type time = int
    type dirty = bool
    module Queue = Map_int
    module Pmap = Cache.Pmap
    type ('k, 'v) cache_state =
      ('k, 'v) Cache.cache_state = {
      max_size : int;
      evict_count : int;
      current : time;
      map : ('k, 'v option * time * dirty) Pmap.t;
      queue : 'k Queue.t;
    }
    type ('k, 'v, 't) cache_ops =
        (('k, 'v) cache_state, 't) mref
    val make_cached_map :
      map_ops:('a, 'b, 'c) Map_ops.map_ops ->
      cache_ops:(('a, 'b) cache_state, 'c) mref ->
      (cached_map_ops:('a, 'b, 'c) Map_ops.map_ops ->
       evict_hook:(unit -> unit) ref -> 'd) ->
      'd
  end
