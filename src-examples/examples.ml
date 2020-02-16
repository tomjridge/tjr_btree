(** Various examples *)

(* FIXME need to recode all the functionality from 7dd9b63 *)

open Make_example

(** 
{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 
%}

*)

module type S = sig 
    type k[@@deriving bin_io]
    type v[@@deriving bin_io]
    val k_cmp: k -> k -> int
    val cs : constants
  end

module Make(S:S) = struct
  include Make(struct
      include S
      type blk_id = Blk_id_as_int.blk_id[@@deriving bin_io]
      type r      = blk_id[@@deriving bin_io]
      let r_cmp: r -> r -> int = Pervasives.compare
      type blk    = ba_buf
    end)
end

type arg = 
  | A1_int_int

module Make_1() = struct
  include Make(struct
      open Bin_prot.Std
      type k = int[@@deriving bin_io]
      type v = int[@@deriving bin_io]
      let k_cmp: k -> k -> int = Pervasives.compare
      let cs = Bin_prot_marshalling.make_constants ~blk_sz:blk_sz_4096 ~k_size:9 ~v_size:9 (* FIXME constants should be part of a factory *)
    end)
end

