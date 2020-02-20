(** Various examples *)

(* FIXME need to recode all the functionality from 7dd9b63 *)
open Intf_
open Make_example

(**/**)
module Pvt = struct
  module Int_int = struct
    open Bin_prot.Std
    type k = int[@@deriving bin_io]
    type v = int[@@deriving bin_io]
    let k_cmp: k -> k -> int = Pervasives.compare
    let cs = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size:9 ~v_size:9 (* FIXME constants should be part of a factory *)
    let debug_k_and_v_are_int = true
  end
end
open Pvt
(**/**)

module type INT_INT_EX = EX with type k=int and type v=int and type t=lwt

module Int_int_ex : INT_INT_EX = Make(Int_int)

