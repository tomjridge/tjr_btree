(** A collection of the main interfaces. This should be read first.

NOTE that this uses some terminology established in package [isa_btree].
 *)


(** A type for recording the marshalling functions *)
(* $(PIPE2SH("""sed -n '/type[ ].*dnode_mshlr/,/}/p' >GEN.dnode_mshlr.ml_""")) *)
type ('dnode,'blk) dnode_mshlr = {
  dnode_to_blk : 'dnode -> 'blk;
  blk_to_dnode : 'blk -> 'dnode;
  blk_sz       : blk_sz
}


(** It is common for marshalling to go via a representation using
    lists. This allows us to assume little about node and leaf
    implementations. These fields have the same names and types as
    the corresponding fields in node_ops and leaf_ops. *)
(* $(PIPE2SH("""sed -n '/type[ ].*node_cnvs/,/}/p' >GEN.node_cnvs.ml_""")) *)
type ('k,'v,'r,'node,'leaf) node_cnvs = {
  node_to_krs: 'node -> 'k list * 'r list;
  krs_to_node: ('k list * 'r list) -> 'node;
  leaf_to_kvs: 'leaf -> ('k * 'v) list;
  kvs_to_leaf: ('k * 'v) list -> 'leaf;
}


(** The B-tree code exports a [pre_map_ops] version of a map, with
   explicit passing of the reference to the root of the B-tree. In
   order to implement the [map_ops] interface, we need to store the
   "current" reference to the B-tree root in the global state
   somehow. A value of type [('r,'t) btree_root_ops] reveals how to read and
   write this reference in the global state.  
*)

(** Btree root; just an int/blk_id *)
type ('r,'t) btree_root_ops = ('r,'t)with_state


(** As map ops, but with leaf_stream operations *)
module Map_ops_with_ls = struct

  (* $(PIPE2SH("""sed -n '/type[ ].*map_ops_with_ls = {/,/}/p' >GEN.map_ops_with_ls.ml_""")) *)
  type ('k,'v,'r,'ls,'t) map_ops_with_ls = {
    find            : k:'k -> ('v option, 't)m;
    insert          : k:'k -> v:'v -> (unit,'t) m;
    delete          : k:'k -> (unit,'t)m;
    insert_many     : k:'k -> v:'v -> kvs:('k * 'v) list -> (('k * 'v) list, 't) m;
    insert_all      : kvs:('k * 'v) list -> (unit, 't) m;    
    leaf_stream_ops : ('k,'v,'r,'ls,'t)Isa_btree_intf.leaf_stream_ops;
  }

end
type ('k,'v,'r,'ls,'t) map_ops_with_ls = 
  ('k,'v,'r,'ls,'t) Map_ops_with_ls.map_ops_with_ls

module Disk_ops_type = struct
  
  (* $(PIPE2SH("""sed -n '/A[ ]collection of block-based/,/}/p' >GEN.disk_ops.ml_""")) *)
  (** A collection of block-based interfaces *)
  type ('r,'t,'dnode,'blk) disk_ops = {
    dnode_mshlr : ('dnode,'blk)dnode_mshlr;
    blk_dev_ops : ('r,'blk,'t)blk_dev_ops;
    blk_alloc   : ('r,'t)blk_allocator_ops
  }

end
include Disk_ops_type


(** {2 Class interfaces} *)


(* NOTE previously in make_3 *)

type finished = {finished:bool}

class type ['k, 'v, 't ] ls = object
  method ls_step: unit -> (finished,'t)m
  method ls_kvs: unit -> ('k*'v)list
end

(** Convert a standard ls to a class interface *)
let ls2object 
    ~monad_ops 
    ~(leaf_stream_ops:(_,_,_,_,_)Isa_btree.Isa_btree_intf.leaf_stream_ops) 
    ~get_r 
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  fun () ->        
    get_r () >>= fun r ->
    leaf_stream_ops.make_leaf_stream r >>= fun ls ->     
    let ls = ref ls in
    let ls_step () = (leaf_stream_ops.ls_step) !ls >>= fun (x:_ option) -> 
      match x with
      | None -> return {finished=true}
      | Some x -> ls:=x; return {finished=false} 
    in
    let ls_kvs () = leaf_stream_ops.ls_kvs !ls in
    let obj : (_,_,_) ls = object method ls_step=ls_step method ls_kvs=ls_kvs end in
    return obj


(** NOTE uncached here means "no write back cache"; a read cache is
   not observable except for performance and memory usage *)    
class type ['k, 'v, 't ] uncached_btree = 
  object
    method find        : 'k -> ('v option,'t)m
    method insert      : 'k -> 'v -> (unit,'t)m
    method delete      : 'k -> (unit,'t)m
    (* method insert_many : ('k*'v) -> ('k*'v)list -> (('k*'v)list,'t)m *)
    (* method insert_all  : ('k*'v)list -> (unit,'t)m *)
    method ls_create   : unit -> (('k,'v,'t)ls,'t)m
    (* method empty_leaf_as_blk: unit -> ba_buf *)
  end 


(** {2 For [Make_5] } *)

type ('a,'t) with_btree_root = ('a,'t)with_state
