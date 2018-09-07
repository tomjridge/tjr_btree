open Tjr_btree
open Isa_export

(* FIXME Res includes Pervasives, but shouldn't - or else put the
   result type in a submodule *)
(* module Res = struct include Res end *)

module Key_value = struct include Key_value end

module Prelude = struct include Prelude end

module Searching_and_splitting = struct include Searching_and_splitting end

module Tree = struct include Tree end

module Tree_stack = struct include Tree_stack end

module Find_state = struct include Find_state end

module Disk_node = struct include Disk_node end

module Leaf_stream_state = struct include Leaf_stream_state end

module Insert_many_state = struct include Insert_many_state end

module Insert_state = struct include Insert_state end

module Delete_state = struct include Delete_state end

module Pre_params = struct include Pre_params end

module Params = struct include Params end

module type MONAD = sig include MONAD end

module Make(Monad:MONAD) = struct include Isa_export.Make(Monad) end
(*
module Find = struct include Find end

module Pre_insert = struct include Pre_insert end

module Insert = struct include Insert end

module Delete2 = struct include Delete2 end

module Insert_many = struct include Insert_many end

module Leaf_stream = struct include Leaf_stream end
*)

