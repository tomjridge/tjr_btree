(** Export the main functionality of this library. 

Usage: open Tjr_btree;; open Tjr_btree.Btree_intf;; 
*)

(** 
{%html: 
<img src="https://docs.google.com/drawings/d/e/2PACX-1vSbPmP9hfqwpYdJefrAYVY_7nSf6Mf5kzAXHYEaaAbw6cLwkWJH9GImYG_4KwKRDLOOjDGMvePbodwt/pub?w=1137&amp;h=766"> 
%}

*)

(** {2 Main interfaces} *)

module Btree_intf = Btree_intf

(** {2 Disk_to_store} *)

include Disk_to_store


(** {2 Store to pre-btree} *)

(** This is provided by the {!Isa_btree} package. *)


(** {2 Pre-btree to map} *)

include Pre_btree_to_map


(** {2 Main functionality: disk to map} *)

module Make_1 = Make_1
include Make_1

(** {2 Alternative version, using binprot, with simpler intf} *)

module Make_2 = Make_2
