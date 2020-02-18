(** Various examples *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Make_example

type arg = 
  | A1_int_int

module Int_int = struct
  open Bin_prot.Std
  type k = int[@@deriving bin_io]
  type v = int[@@deriving bin_io]
  let k_cmp: k -> k -> int = Pervasives.compare
  let cs = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size:9 ~v_size:9 (* FIXME constants should be part of a factory *)
  let debug_k_and_v_are_int = true
end

module Make_1() = Make(Int_int)

