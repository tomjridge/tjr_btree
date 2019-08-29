(** Various examples *)
(* open Tjr_profile.Util.Profiler *)
open Tjr_btree
open Btree_intf
open Intf_
(* open Fstore_layer *)

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
type ('k,'v,'r,'t,'blk_id,'blk,'fd,'node,'leaf,'leaf_stream,'extra) example = {
  monad_ops         : 't monad_ops;
  compare_k         : 'k -> 'k -> int;
  blk_ops           : 'blk blk_ops;
  blk_dev_ops       : 'fd -> ('blk_id,'blk,'t)blk_dev_ops;
  blk_allocator_ref : 'blk_id blk_allocator_state ref; 
  blk_allocator     : ('blk_id blk_allocator_state,'t)with_state;
  reader_writers    : ('k,'v)Bin_prot_marshalling.reader_writers;
  nlc               : ('k,'v,'r,'node,'leaf) nlc;
  marshalling_ops   : (('node,'leaf)dnode,'blk) marshalling_ops;
  disk_ops          : 'fd -> ('blk_id,'t,('node,'leaf)dnode,'blk) disk_ops;
  store_ops         : 'fd -> ('blk_id,('node,'leaf)dnode,'t) store_ops;
  pre_btree_ops     : 'fd -> ('k,'v,'blk_id,'t,'leaf,'node,'leaf_stream) pre_btree_ops;
  btree_root_ref    : 'blk_id btree_root_state ref;
  btree_root_ops    : ('blk_id btree_root_state,'t)with_state;
  map_ops_with_ls   : 'fd -> ('k,'v,'r,'leaf_stream,'t)map_ops_with_ls;
  empty_leaf_as_blk : 'blk; 
  extra             : 'extra
}
(* NOTE the use of refs here means we need to allocate a new ref for
   each example instance. We use refs essentially so that we have an
   easy set/get method for initialization/finalization. *)


(* FIXME move this to tjr_monad.imp *)
let with_imperative_ref ~monad_ops = 
  let return = monad_ops.return in
  fun r -> 
    let with_state f = 
      f ~state:(!r) ~set_state:(fun r' -> r:=r'; return ())
    in
    { with_state }

module Inc1 = struct
  type blk = string (* FIXME shield *)
  type r = blk_id
  let blk_ops = Common_blk_ops.string_blk_ops
  let blk_sz = blk_ops.blk_sz
end

module type S1 = sig
  type r = blk_id
  type blk
  val blk_ops: blk blk_ops
  val blk_sz: blk_sz
  type k
  type v
  val k_cmp: k -> k -> int
  val k_size: int
  val v_size: int
  val cs: constants
  val reader_writers: (k,v) Bin_prot_marshalling.reader_writers
end

module Without_monad = struct
  module Int_int = struct
    include Inc1
    open Bin_prot_marshalling
    type k = int
    type v = int
    let k_cmp = Int_.compare
    let k_size=int_bin_prot_info.max_size
    let v_size=int_bin_prot_info.max_size
    let cs = Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size 
    let reader_writers = Bin_prot_marshalling.Common_reader_writers.int_int
  end

  module String_string = struct
    include Inc1
    open Bin_prot_marshalling
    type k = ss
    type v = ss
    let k_cmp = Small_string.compare
    let k_size=ss_bin_prot_info.max_size
    let v_size=ss_bin_prot_info.max_size
    let cs = Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size     
    let reader_writers = Bin_prot_marshalling.Common_reader_writers.ss_ss
  end
end


module type S2 = sig
  include S1
  type t
  type fd
  val monad_ops: t monad_ops
  val blk_dev_ops: fd -> (blk_id,blk,t)blk_dev_ops
  type node
  type leaf
  val leaf_ops : (k,v,leaf)Isa_btree_intf.leaf_ops
  (* val empty_leaf_as_blk: (node,leaf)dnode -> blk *)
  type leaf_stream
  val node_leaf_list_conversions: (k,v,r,node,leaf) nlc
  type nonrec store_ops = (blk_id,(node,leaf)dnode,t)store_ops
  type nonrec pre_btree_ops = (k,v,blk_id,t,leaf,node,leaf_stream) pre_btree_ops
  val store_to_pre_btree : store_ops:store_ops -> pre_btree_ops
end

module Make(S:S2) = struct
  open S
  let make () = 
    let compare_k = k_cmp in
    let blk_allocator_ref = ref { min_free_blk_id=(Blk_id.of_int (-1)) } in
    (* NOTE we assume the -1 value is never used *) 
    let blk_allocator = with_imperative_ref ~monad_ops blk_allocator_ref in
    let blk_allocator_ops = Blk_layer.make_blk_allocator_ops
        ~monad_ops ~blk_allocator in
    let reader_writers = reader_writers in
    let nlc = node_leaf_list_conversions in
    let marshalling_ops = Blk_layer.make_marshalling_ops ~blk_ops
        ~node_leaf_list_conversions
        ~reader_writers
    in  
    let disk_ops fd = 
      Blk_layer.make_disk_ops ~marshalling_ops
        ~blk_dev_ops:(blk_dev_ops fd) ~blk_allocator_ops in
    let store_ops fd =
      disk_to_store ~monad_ops ~disk_ops:(disk_ops fd)
      |> fun store_ops -> Store_cache.add_imperative_read_cache_to_store ~monad_ops ~store_ops
    in
    let pre_btree_ops fd = 
      store_to_pre_btree ~store_ops:(store_ops fd)
    in
    let btree_root_ref = ref { btree_root=(Blk_id.of_int (-1)) } in
    let btree_root_ops = with_imperative_ref ~monad_ops btree_root_ref in
    let map_ops_with_ls fd =
      let pre_btree_ops = pre_btree_ops fd in
      pre_btree_to_map ~monad_ops ~pre_btree_ops ~root_ops:btree_root_ops
    in
    let empty_leaf_as_blk = marshalling_ops.dnode_to_blk (Disk_leaf (leaf_ops.kvs_to_leaf [])) in
    (* FIXME empty_leaf_as_blk in Tjr_btree should just be a blk *)
    {monad_ops; compare_k; blk_ops; blk_dev_ops; blk_allocator_ref;
     blk_allocator; reader_writers; nlc; marshalling_ops; disk_ops;
     store_ops; pre_btree_ops; btree_root_ref; btree_root_ops; map_ops_with_ls; empty_leaf_as_blk;
     extra=()}
end


module Imperative = struct
  open Inc1
  module Inc2 = struct
    type fd = Unix.file_descr

    type t = imperative
    let monad_ops = imperative_monad_ops

    let read_count = ref 0
    let write_count = ref 0 
    let _ = Pervasives.at_exit (fun () -> 
        Printf.printf "Block statistics: %d read; %d written\n" (!read_count) (!write_count))

    let blk_dev_ops fd = 
      Blk_dev_on_fd.make_with_unix ~monad_ops ~blk_ops ~fd |> fun { blk_sz; read; write } -> 
      { blk_sz; 
        read=(fun ~blk_id -> incr read_count; read ~blk_id);
        write=(fun ~blk_id ~blk -> incr write_count; write ~blk_id ~blk)
      }
  end

  module Int_int = struct    
    module S = struct
      include Without_monad.Int_int          
      include Inc2
    end
    include S
    module Btree = Tjr_btree.Make(S)    
    include Btree
  end

  let int_int_example () = 
    let module A = Make(Int_int) in
    A.make ()

  let _ 
: unit ->
(int, int, blk_id, imperative, blk_id, string, Unix.file_descr,
 Int_int.Btree.node, Int_int.Btree.leaf, Int_int.Btree.leaf_stream, unit)
example
= int_int_example


  (* NOTE this is essentially a cut-n-paste of the int-int example *)
  module String_string = struct    
    module S = struct
      include Without_monad.String_string
      include Inc2
    end
    include S
    module Btree = Tjr_btree.Make(S)
    include Btree
  end

  let ss_ss_example () = 
    let module A = Make(String_string) in
    A.make ()
end


(* NOTE this is a cut-n-paste of the imperative version *)
module Lwt = struct
  open Inc1
  module Inc2 = struct
    type fd = Lwt_unix.file_descr
        
    type t = lwt
    let monad_ops = lwt_monad_ops

    let blk_dev_ops fd = 
      Blk_dev_on_fd.make_with_lwt ~blk_ops ~fd
  end

  (* FIXME move this to tjr_monad.imp *)
  let with_imperative_ref ~monad_ops = 
    let return = monad_ops.return in
    fun r -> 
      let with_state f = 
        f ~state:(!r) ~set_state:(fun r' -> r:=r'; return ())
      in
      { with_state }

  module Int_int = struct    
    module S = struct
      include Without_monad.Int_int
      include Inc2
    end
    include S
    module Btree = Tjr_btree.Make(S)
    include Btree
  end

  let int_int_example () = 
    let module A = Make(Int_int) in
    A.make ()

  let _ 
: unit ->
(int, int, blk_id, lwt, blk_id, string, Lwt_unix.file_descr,
 Int_int.Btree.node, Int_int.Btree.leaf, Int_int.Btree.leaf_stream, unit)
example
= int_int_example

  module String_string = struct    
    module S = struct
      include Without_monad.String_string
      include Inc2
    end
    include S
    module Btree = Tjr_btree.Make(S)
    include Btree
  end

  let ss_ss_example () = 
    let module A = Make(String_string) in
    A.make ()

end
