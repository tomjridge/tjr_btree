(* Pick out the main modules, functors etc *)


(* base_types ------------------------------------------------------- *)

open Block
module BlkN = struct include BlkN end

module Constants = struct include Constants end  (* FIXME just t, not defs *)

module Frame = struct include Frame end

module Ls_state = struct include Ls_state end

(* FIXME params should probably be real types rather than object
   projections; at least need to ensure object projs aren't having a
   negative performance impact *)

module Params = struct include Params end

module R2f = struct include R2f end

module R2t = struct include R2t end

module Rstk = struct include Rstk end

module Tree = struct include Tree end


(* api -------------------------------------------------------------- *)

module Disk_ops = struct include Disk_ops end  (* FIXME types! *)

module Leaf_stream_ops = struct include Leaf_stream_ops end

module Map_ops = struct include Map_ops end

module Pre_map_ops = struct include Pre_map_ops end


(* FIXME want to include just the type defn as type x = Y.z = ... but
   following doesn't expand the ... in the generated .mli *)

(*
module type X = sig
type ('k, 'v, 'r, 't) store_ops =
      ('k, 'v, 'r, 't) Store_ops.store_ops
end
*)

module Store_ops = struct include Store_ops end

module Small_step_types = struct include Small_step.O end  (* FIXME naming *)

(* FIXME in recap we also want to include main functions eg store_to_map *)

module Store_to_map = struct include Store_to_map end


(* disks ------------------------------------------------------------ *)

module Disk_on_fd = struct include Disk_on_fd end

module Disk_to_store = struct include Disk_to_store end


(* stores ----------------------------------------------------------- *)

module Mem_store = struct include Mem_store end


(* cache ------------------------------------------------------------ *)

module Cache = struct include Cache end
