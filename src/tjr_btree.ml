(** Export the main functionality of this library. 

Usage: open Tjr_btree;; open Tjr_btree.Btree_intf;; 
*)

(** 
{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 
%}

*)

include Summary

(** {2 Main interfaces} *)

module Btree_intf = Btree_intf

(* module Btree_intf_v2 = Btree_intf_v2 *)


module Pvt = struct
  (** {2 Store read cache} *)

  module Store_read_cache = Store_read_cache


  (** {2 Store write back cache} *)

  module Store_write_back_cache = Store_write_back_cache


  (** {2 Disk_to_store} *)

  module Disk_to_store = Disk_to_store


  (** {2 Store to pre-btree} *)

  (** This is provided by the {!Isa_btree} package. *)


  (** {2 Pre-btree to map} *)

  module Pre_btree_to_map = Pre_btree_to_map



  (** {2 Alternative version, using binprot, with simpler intf} *)

  module Make_2 = Make_2


  (** {2 Alternative version, using binprot, with objects and no type generation} *)

  module Make_3 = Make_3

  module Make_5 = Make_5

end


module Make_1 = Make_1

module Make_6 = Make_6

type ('k,'v,'r,'t,'leaf,'node,'dnode,'ls,'blk,'wbc) btree_factory = 
  ('k,'v,'r,'t,'leaf,'node,'dnode,'ls,'blk,'wbc) Make_6.btree_factory

let btree_examples = object
  method int_int_factory = Make_6.Examples.int_int_factory
  method int_r_factory = Make_6.Examples.int_r_factory
end

(** {2 Bin prot marshalling} *)

module Bin_prot_marshalling = Bin_prot_marshalling

(** {2 Misc} *)

module Debug_ = Debug_
