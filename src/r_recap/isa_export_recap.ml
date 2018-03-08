open Tjr_btree
open Isa_export

(* FIXME Res includes Pervasives, but shouldn't - or else put the
   result type in a submodule *)
(* module Res = struct include Res end *)

module Prelude = struct include Prelude end

module Tree = struct include Tree end

module Tree_stack = struct include Tree_stack end

module Disk_node = struct include Disk_node end

module Params = struct include Params end

module Find = struct include Find end

module Pre_insert = struct include Pre_insert end

module Insert = struct include Insert end

module Delete2 = struct include Delete2 end

module Insert_many = struct include Insert_many end

module Leaf_stream = struct include Leaf_stream end


