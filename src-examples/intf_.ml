

(** Standard types: t = lwt; blk=ba_buf; r=blk_id *)
module Std_types = struct
  type t = lwt
  type blk = ba_buf
  type blk_id = Blk_id_as_int.blk_id[@@deriving bin_io]
  type r = blk_id
end
open Std_types

(*

(** Flags:

- O_TRUNC, reinitialize B-tree root to point to an empty leaf; possibly also truncate the underlying file
- O_NOCACHE, do not use a write back cache (the underlying file may still need to be sync'ed tho)
*)
type flg = O_TRUNC | O_NOCACHE


(** The specific rt_blk type, a pair of refs (the B-tree root, and the
   blk_alloc min free blk) *)
module Rt_blk_type = struct
  open Bin_prot.Std
  type rt_blk = {
    bt_rt:blk_id ref;
    blk_alloc:blk_id ref
  }[@@deriving bin_io]    
end
open Rt_blk_type

(** The values available after opening a store; NOTE that this depends
   on empty_leaf_as_blk in order to initialize an empty btree leaf *)
module type FROM_OPEN = 
sig
  val fd               : Lwt_unix.file_descr
  val blk_dev_ops      : (blk_id, ba_buf,lwt)blk_dev_ops
  val rt_blk           : rt_blk

  (** Close writes the root blk, then closes the fd; NOTE it does not
     flush the cache *)
  val wrt_rt_and_close : unit -> (unit, lwt)m

  val blk_alloc        : (blk_id, lwt) blk_allocator_ops
  val root_ops         : (blk_id, lwt) with_state
end


module type FROM_BTREE = 
sig
  type k
  type v
  type t = lwt
  type blk = ba_buf
  type r = blk_id
  type ls
  val empty_leaf_as_blk : unit -> blk
  module type S = sig
    val write_empty_leaf: blk_id -> (unit,t) m
    val flush_cache : unit -> (unit, t) Tjr_monad.m
    val map_ops_with_ls :
      (k, v, r, ls, t) Tjr_btree.Btree_intf.map_ops_with_ls
  end
  val make :
    blk_dev_ops:(r, blk, t) Tjr_fs_shared.blk_dev_ops ->
    blk_alloc:(r, t) Tjr_fs_shared.blk_allocator_ops ->
    root_ops:(r, t) Tjr_btree.Btree_intf.btree_root_ops -> (module S)
end


(** Combination of FROM_BTREE and FROM_OPEN, moving map_ops to top level *)
module type FROM_OPEN_BTREE = 
sig
  type ('a,'t) m = ('a,'t) Tjr_monad.m
  module From_open : FROM_OPEN
  open Std_types
  type k
  type v
  type ls
  val flush_cache : unit -> (unit, t) m
  val find        : k:k -> (v option, t) m
  val insert      : k:k -> v:v -> (unit, t) m
  val insert_many : k:k -> v:v -> kvs:(k * v) list -> ((k * v) list, t) m
  val insert_all  : kvs:(k * v) list -> (unit, t) m
  val delete      : k:k -> (unit, t) m
  val ls_create   : unit -> (ls,t)m
  val ls_step     : ls -> (ls option, t) m
  val ls_kvs      : ls -> (k * v) list
      
  (** close: flush the cache, write the root block, and close the fd *)
  val close       : unit -> (unit,t) m
end

(* type ('k,'v,'ls) btree = (module FROM_OPEN_BTREE with type k='k and type v='v and type ls='ls) *)
*)


(** {2 Interfaces based on classes and objects} *)

class type ['k, 'v, 'ls ] btree = 
  object
    (* val bt_rt         : blk_id ref *)
    (* method with_bt_rt       : (blk_id,t)with_state *)
    (* method blk_dev_ops      : (blk_id,blk,t) blk_dev_ops *)
    method map_ops          : ('k,'v,r,'ls,t)map_ops_with_ls
    method flush_cache      : unit -> (unit,t)m
    (* method write_empty_leaf : blk_id -> (unit,t)m *)
    method ls_create : unit -> ('ls,t)m
    method ls_step : 'ls -> ('ls option,t)m
    method ls_kvs  : 'ls -> ('k*'v) list
  end 

class type open_fd = 
  object
    method fd          : Lwt_unix.file_descr
    method blk_dev_ops : (blk_id,blk,t) blk_dev_ops
    method close_fd    : unit -> (unit,t)m
  end
    
(** NOTE: typically one of the init functions must be called *)
class type rt_blk = 
  object
    method init_from_disk   : unit -> (unit,t)m
    method init_as_empty    : empty_leaf_as_blk:(unit -> blk) -> (unit,t)m
    method blk_dev_ops      : (blk_id,blk,t) blk_dev_ops
    method with_bt_rt       : (blk_id,t) with_state
    method blk_alloc        : (blk_id, lwt) blk_allocator_ops
    method sync             : unit -> (unit,t)m
  end

    (* method bt_rt_ref     : blk_id ref (\** equal to the single b-tree rt *\) *)
    (* method blk_alloc_ref : blk_id ref *)

