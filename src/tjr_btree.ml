(** Export the main functionality of this library. 

Usage: open Tjr_btree;; open Tjr_btree.Btree_intf;; 
*)

(** 
{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 
%}

*)

(** {2 Main interfaces} *)

module Btree_intf = Btree_intf

(* module Btree_intf_v2 = Btree_intf_v2 *)


module Pvt = struct
  (** {2 Store read cache} *)

  module Store_read_cache = Store_read_cache


  (** {2 Store write back cache} *)

  module Store_write_back_cache = Store_write_back_cache


  (** {2 Disk_to_store} *)

  module Disk_to_store = Disk_to_store


  (** {2 Store to pre-btree} *)

  (** This is provided by the {!Isa_btree} package. *)


  (** {2 Pre-btree to map} *)

  module Pre_btree_to_map = Pre_btree_to_map



  (** {2 Alternative version, using binprot, with simpler intf} *)

  module Make_2 = Make_2


  (** {2 Alternative version, using binprot, with objects and no type generation} *)

  module Make_3 = Make_3


end


module Make_1 = Make_1

module Make_5 = Make_5

(** [Make_5] main interfaces:

{[
module type S = sig
  type k
  type v
  type r
  type t
  val k_cmp: k -> k -> int
  val monad_ops: t monad_ops
  val cs: Constants.constants

  val k_mshlr: k bp_mshlr
  val v_mshlr: v bp_mshlr
  val r_mshlr: r bp_mshlr

  val r_cmp: r -> r -> int (* for wbc *)
end

  type ('k,'v,'r,'ls,'t) bt_1 = <
    map_ops_with_ls: ('k,'v,'r,'ls,'t) map_ops_with_ls
  >

  type ('k,'v,'r,'ls,'t) bt_2 = <
    flush_wbc: unit -> (unit,'t)m;
    sync_key: 'k -> (unit,'t)m;
    map_ops_with_ls: ('k,'v,'r,'ls,'t) map_ops_with_ls
  >

  type ('k,'v,'r,'t,'ls,'blk,'dnode,'wbc) btree_factory = <
    (* method blk_dev_ops: ('r,'blk,'t) blk_dev_ops *)
    (* method blk_allocator_ops: ('r,'t)blk_allocator_ops *)
    empty_leaf_as_blk: 'blk;
    wbc_factory: ('r,'dnode,'wbc)wbc_factory;
    make_uncached: ('r, 't) with_btree_root -> ('k,'v,'r,'ls,'t) bt_1;
    make_cached_1: ('r, 't) with_btree_root -> ('wbc,'t)with_state -> ('k,'v,'r,'ls,'t) bt_2;
    make_cached_2: ('r, 't) with_btree_root -> ('k,'v,'r,'ls,'t) bt_2;
  >

]}

*)


(** {2 Bin prot marshalling} *)

module Bin_prot_marshalling = Bin_prot_marshalling

(** {2 Misc} *)

module Debug_ = Debug_
