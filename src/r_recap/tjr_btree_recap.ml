open Tjr_btree
(* Pick out the main modules, functors etc *)

(* from isa_btree, which is not packed *)
module Constants_type = struct include Constants.Constants_type end 


(* base_types ------------------------------------------------------- *)

module type Blk_types = sig include Block.Blk_types end

module Frame_type = struct include Frame.Frame_type end


(* NOTE ls_state is abstract in isa_export *)
module Ls_state = struct include Ls_state end

(* FIXME params should probably be real types rather than object
   projections; at least need to ensure object projs aren't having a
   negative performance impact *)

module Params = struct include Params end

module R2f = struct include R2f end

module R2t = struct include R2t end

module Rstk = struct include Rstk end

module Tree_type = struct include Tree.Tree_type end


(* api -------------------------------------------------------------- *)

module Disk_ops = struct include Disk_ops end  (* FIXME types! *)

module Leaf_stream_ops = struct include Leaf_stream_ops end

module Pre_map_ops = struct include Pre_map_ops end

module Map_ops = struct include Map_ops end


(* FIXME want to include just the type defn as type x = Y.z = ... but
   following doesn't expand the ... in the generated .mli *)

(*
module type X = sig
type ('k, 'v, 'r, 't) store_ops =
      ('k, 'v, 'r, 't) Store_ops.store_ops
end
*)

module Store_ops = struct include Store_ops end

(* module Small_step_types = struct include Small_step. end  (* FIXME naming *) *)

(* FIXME in recap we also want to include main functions eg store_to_map *)

(* store to map ----------------------------------------------------- *)

module Store_to_map = struct include Store_to_map end


(* disks ------------------------------------------------------------ *)

module Disk_on_fd = struct include Disk_on_fd end

module Disk_to_store = struct include Disk_to_store end


(* stores ----------------------------------------------------------- *)

module Mem_store = struct include Mem_store end


(* cache ------------------------------------------------------------ *)

module Cache = struct include Cache end
