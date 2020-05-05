(** Interfaces based on using classes and inheritance to make the
   system structure clearer *)

(**

{%html:
<img width='100%' src="https://docs.google.com/drawings/d/e/2PACX-1vRaddM5Jrk-qNGkt1wH4-bQE2izLIe3BSuN-WqC0hePBForvaOBhvYMifGpADFdgMA7iyrAOYBSeeb_/pub?w=1474&amp;h=1401">


%}

*)

open Btree_intf

(** {2 c1 and c2, basic types and ops} *)

class virtual ['k,'v,'r,'t] c1 = object
  method virtual monad_ops: 't monad_ops
  method virtual k_cmp: 'k -> 'k -> int
  method virtual cs: constants    
end


class virtual ['k,'v,'r,'t,'leaf,'node,'ls] c2 = object
  inherit ['k,'v,'r,'t]c1
  (* method virtual leaf_ops: ('k,'v,'leaf)leaf_ops *)
  (* method virtual node_ops: ('k,'v,'node)node_ops *)
  method virtual node_cnvs: ('k,'v,'r,'node,'leaf)node_cnvs

  (* FIXME we may want this to be "uncached", or else make other
     operations parametric on store_ops *)
  method virtual store_ops: ('r,('node,'leaf)dnode,'t)store_ops

  (** This uses the store_ops; FIXME FIXME how to get this to use uncached or cached? *)
  method virtual pre_btree_ops: ('k, 'v, 'r, 't, 'leaf, 'node, 'ls)pre_btree_ops
end



(** {2 c3 is c2 + with_btree_root} *)

type ('a,'t) with_btree_root = ('a,'t)with_state

class virtual ['k,'v,'r,'t,'leaf,'node,'ls]c3 = object
  inherit ['k,'v,'r,'t,'leaf,'node,'ls]c2
  method virtual with_btree_root : ('r,'t)with_btree_root (* RUNTIME *)
  method virtual map_ops_with_ls: ('k,'v,'r,'ls,'t) map_ops_with_ls
end


(** {2 c4 bridges store and disk} *)

class virtual ['k,'v,'r,'t,'leaf,'node,'ls,'blk] c4 = object
  
  method virtual dnode_mshlr: (('node,'leaf)dnode,'blk) dnode_mshlr 
  method virtual blk_dev_ops: ('r,'blk,'t)blk_dev_ops (* RUNTIME *)
  method virtual blk_alloc: ('r, 't) blk_allocator_ops (* RUNTIME *)

  (** store_ops uses the disk ops, marshalling and blk_alloc *)
  (* method virtual store_ops: ('r,('node,'leaf)dnode,'t)store_ops *)

  (* this is the raw store ops, which works directly with the disk *)
  method virtual uncached_store_ops: ('r,('node,'leaf)dnode,'t)store_ops
end



(** {2 c5 is binprot marshalling; only for 'blk=ba_buf} *)

module type TYPE_WITH_MSHLR = sig
  type t[@@deriving bin_io]
end

type 'a type_with_mshlr = (module TYPE_WITH_MSHLR with type t='a) 

class virtual ['k,'v,'r,'t,'leaf,'node,'blk] c5 = object
  method virtual blk_sz: blk_sz
  method virtual node_cnvs: ('k,'v,'r,'node,'leaf)node_cnvs

  method virtual kvr_mshlrs: 
    'k type_with_mshlr * 
    'v type_with_mshlr *
    'r type_with_mshlr

  (** uses node_cnvs and bin_prot marshalling on types k v r t *)
  method virtual dnode_mshlr: (('node,'leaf)dnode,'blk) dnode_mshlr
end



(*
(* c6 uses c4 and c3, together with c5 bin_prot, to introduce the dnode_mshlr *)

class virtual ['k,'v,'r,'t,'leaf,'node,'ls,'blk] c6 = object
  inherit ['k,'v,'r,'t,'leaf,'node,'ls]c3
  inherit ['k,'v,'r,'t,'leaf,'node,'ls,'blk]c4
  inherit ['k,'v,'r,'t,'leaf,'node,'blk]c5

  (* NOTE c5 provides the dnode_mshlr *)
end
*)


(** {2 c7 is c4 + write-back cache} *)

(* To introduce the wbc, we can use the store -> disk refinement, and
   add the cache there, so that the store_ops are cached *)

type ('k,'v,'wbc) wbc_ops = ('k,'v,'wbc) Write_back_cache.wbc_ops

class virtual ['k,'v,'r,'t,'leaf,'node,'ls,'blk,'wb] c7 = object
  inherit ['k,'v,'r,'t,'leaf,'node,'ls,'blk]c4
  method virtual write_back_cache_ops:('r, ('node,'leaf)dnode, 'wb) wbc_ops

  method virtual with_write_back_cache: ('wb,'t) with_state

  (* we fulfill the store ops with a version that uses the cache *)
  method virtual store_ops: ('r,('node,'leaf)dnode,'t) store_ops
      
end


(** {2 With fixed types} *)

(** Same classes as above, but with no type parameters *)

module type S = sig
  type k
  type v 
  type r
  type t
  type blk
  type leaf
  type node
  type ls
  type wb
end


module Make(S:S) = struct
  open S

  module Abbrevs = struct
    (* type nonrec leaf_ops = (k,v,leaf)leaf_ops *)
    (* type nonrec node_ops = (k,v,node)node_ops *)
    type nonrec node_cnvs = (k,v,r,node,leaf)node_cnvs

    type nonrec dnode = (node,leaf)dnode
    type nonrec store_ops = (r,dnode,t)store_ops
    type nonrec pre_btree_ops = (k, v, r, t, leaf, node, ls)pre_btree_ops
    type nonrec map_ops_with_ls = (k,v,r,ls,t) map_ops_with_ls

    type nonrec blk_dev_ops = (r,blk,t)blk_dev_ops
  end
  open Abbrevs

  class virtual c1 = object
    method virtual monad_ops: t monad_ops
    method virtual k_cmp: k -> k -> int
    method virtual cs: constants    
  end


  class virtual c2 = object
    inherit c1
    (* method virtual leaf_ops: leaf_ops *)
    (* method virtual node_ops: node_ops *)
    method virtual node_cnvs: node_cnvs

    method virtual store_ops: store_ops
    method virtual pre_btree_ops: pre_btree_ops
  end


  (* NOTE we could also consider putting an lru cache on top of map_ops *)
  class virtual c3 = object
    inherit c2
    method virtual with_btree_root : (r,t)with_btree_root (* in *)
    method virtual map_ops_with_ls : map_ops_with_ls (* out *)
  end
      

  class virtual c4 = object
    method virtual dnode_mshlr: (dnode,blk) dnode_mshlr 
    method virtual blk_dev_ops: blk_dev_ops (* in *)
    method virtual blk_alloc: (r, t) blk_allocator_ops (* out *)

    method virtual uncached_store_ops: store_ops
  end


  class virtual c5 = object
    method virtual blk_sz: blk_sz
    method virtual node_cnvs: node_cnvs

    method virtual kvr_mshlrs: 
      k type_with_mshlr * 
      v type_with_mshlr *
      r type_with_mshlr

    method virtual dnode_mshlr: (dnode,blk) dnode_mshlr
  end


  class virtual c7 = object
    inherit c4
    method virtual write_back_cache_ops:(r, dnode, wb) wbc_ops

    method virtual with_write_back_cache: (wb,t) with_state

    (* we fulfill the store ops with a version that uses the cache *)
    method virtual store_ops_with_cache: store_ops (* out *)
  end


  class virtual c8 = object
    inherit c3 
    inherit c5
    inherit c7
  end


end
(**

The final collection of all methods:

{[
    class virtual c8 :
      object
        method virtual blk_alloc : (S.r, S.t) Tjr_fs_shared.blk_allocator_ops
        method virtual blk_dev_ops : Abbrevs.blk_dev_ops
        method virtual blk_sz : Tjr_fs_shared.blk_sz
        method virtual cs : Isa_btree.constants
        method virtual dnode_mshlr :
          (Abbrevs.dnode, S.blk) Tjr_btree__.Btree_intf.dnode_mshlr
        method virtual k_cmp : S.k -> S.k -> int
        method virtual kvr_mshlrs :
          S.k type_with_mshlr * S.v type_with_mshlr * S.r type_with_mshlr
        method virtual map_ops_with_ls : Abbrevs.map_ops_with_ls
        method virtual monad_ops : S.t Tjr_monad.monad_ops
        method virtual node_cnvs : Abbrevs.node_cnvs
        method virtual pre_btree_ops : Abbrevs.pre_btree_ops
        method virtual store_ops : Abbrevs.store_ops
        method virtual store_ops_with_cache : Abbrevs.store_ops
        method virtual uncached_store_ops : Abbrevs.store_ops
        method virtual with_btree_root : (S.r, S.t) with_btree_root
        method virtual with_write_back_cache :
          (S.wb, S.t) Tjr_monad.with_state
        method virtual write_back_cache_ops :
          (S.r, Abbrevs.dnode, S.r * Abbrevs.dnode, S.wb) wbc_ops
      end
]} *)
