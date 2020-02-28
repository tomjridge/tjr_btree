(** Examples for {!Tjr_btree} *)

(** Usage: open Tjr_btree;; open Tjr_btree.Btree_intf;; open Tjr_btree_examples *)

(** WARNING! Documentation produced by odoc is often incorrect. For
   example, destructive substitution is not handled properly, see
   {{:https://github.com/ocaml/odoc/issues/96} this issue}. So please
   take care when interpreting this documentation. *)


include Intf_

(** {2 Refined construction} *)

(** These are more restricted versions of the make functor from
   {!Tjr_btree}. The monad is lwt; the blk_id is int; blk is ba_buf;
   blk_dev_ops are lwt/ba_buf. Marshalling uses binprot and
   information about max size of marshalled types (dnode_mshlr in
   disk_ops). The construction function takes a blk_dev_ops and a
   blk_alloc. *)
module Make_1 = Make_1


(** This is a variation where we expose a "btree descriptor" which is
   a file descriptor+some extra stuff. The interface is also via a
   module rather than a record. This is primarily for standalone use
   in examples, not as part of a bigger system. *)
module Make_2 = Make_2


(** {2 Actual examples} *)

module Examples = Examples

include Examples

(**/**)

(** {2 Internal} *)

(** {2 In-memory store} *)

module Store_in_mem = Store_in_mem

let mk_in_mem_store_ops = Store_in_mem.mk_store_ops


(** {2 Read cache and write-back cache based on LRU} *)

module Store_read_cache = Store_read_cache
module Store_write_back_cache = Store_write_back_cache


(** {2 Bin-prot marshalling for on-disk store} 

Use Jane St. marshalling libraries.
*)

module Bin_prot_marshalling = Bin_prot_marshalling


module Profilers = Profilers_

(**/**)
