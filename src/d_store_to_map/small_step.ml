(** Wrapper functions round functions exported from Isabelle. The main
   functions are [mk_find_state], [find_step], [dest_f_finished],
   [wellformed_find_state] and similar functions for insert, insert
   many, delete and the leaf stream operations. *)

(* FIXME this module a bit horrible *)

open Base_types
open Rstk
(** Sub-modules called O are safe to open in other modules. This
   module contains types which are often used in the rest of the
   code. *)
module O = struct
  (*
  include Store_ops
  include Frame
  include Tree
  include R2t
*)
  open R2f
  open Isa_export
  type ('k,'v,'r) find_state = ('k,'v,'r) Find.find_state
  type ('k,'v,'r) insert_state = ('k,'v,'r) Insert.insert_state
  type ('k,'v,'r) im_state = ('k,'v,'r) Insert_many.ist
  type ('k,'v,'r) delete_state = ('k,'v,'r) Delete2.delete_state
end

include O
open Isa_export
open Store_ops
open Ls_state

(** Translations between Isabelle types and OCaml native types. *)
module X = Isabelle_conversions

let x5 (x,(y,(z,(w,u)))) = (x,y,z,w,u)


(* find ---------------------------------------- *)

(** Construct an initial "find state" given a key and a reference to a B-tree root. *)
let  mk_find_state k r : ('k,'v,'r) find_state = Find.mk_find_state k r

(** Small step the find state: take a find state and return an updated
   find state (in the monad). *)
let find_step ~constants ~cmp ~store_ops : 'fs->('fs,'t) m = (fun fs -> 
    Find.find_step (X.x_ps1 ~constants ~cmp ~store_ops) fs)

(** Check whether we have reached a leaf. Returns the reference to the
   B-tree root (when [mk_find_state] was called), the key we are
   looking for (ditto), a reference to a leaf (that may contain the
   key... if any leaf contains the key, this leaf does) and a the list
   of (key,values) in that leaf. *)
let dest_f_finished fs: ('r * 'k * 'r * 'kvs * ('k,'r) rstk) option = (
    Find.dest_f_finished fs 
    |> (function None -> None | Some  x -> Some (x5 x)))

(*open Params2*)

(* only check if we have access to the relevant r2t and spec_tree *)
(* ASSUMES r2t ps <> None *)
(** Wellformedness check. Assumes access to the "spec tree", the
   global state and the find state.  *)
let wellformed_find_state ~r2t ~cmp : 'tree -> 't -> ('k,'v,'r) find_state -> bool = (
  fun t s fs -> 
    Find.wellformed_find_state 
      (X.x_cmp cmp) r2t t s fs)
  

(* delete ---------------------------------------- *)

(** Similar functionality to [mk_find_state] *)
let mk_delete_state: 'k -> 'r -> ('k,'v,'r) delete_state = 
  Delete2.mk_delete_state

let delete_step ~constants ~cmp ~store_ops : 'ds -> ('ds,'t) m = 
  (fun ds -> Delete2.delete_step (X.x_ps1 ~constants ~cmp ~store_ops) ds)

(** The result is a reference to the updated B-tree *)
let dest_d_finished: ('k,'v,'r) delete_state->'r option = Delete2.dest_d_finished

let wellformed_delete_state ~cmp ~constants ~r2t : 'tree->'t->'k->('k,'v,'r) delete_state->bool = (
  fun t s k ds -> 
    Delete2.wellformed_delete_state 
      (X.x_constants constants) (X.x_cmp cmp)
      r2t t s k ds)
  

(* insert ---------------------------------------- *)

let mk_insert_state: 'k->'v->'r->('k,'v,'r) insert_state = 
  Insert.mk_insert_state

let insert_step ~cmp ~constants ~store_ops : 'is -> ('is,'t) m = 
  (fun is -> Insert.insert_step (X.x_ps1 ~cmp ~constants ~store_ops) is)

(** Result is a reference to the updated B-tree *)
let dest_i_finished: ('k,'v,'r)insert_state -> 'r option = 
  Insert.dest_i_finished

let wellformed_insert_state ~cmp ~constants ~r2t :'tree->'t->'k->'v->('k,'v,'r) insert_state->bool = (
  fun t s k v is ->
    Insert.wellformed_insert_state 
      (X.x_constants constants) (X.x_cmp cmp)
      r2t t s k v is)

(* insert_many ---------------------------------------- *)

let mk_im_state: 'k -> 'v -> ('k*'v) list -> 'r -> ('k,'v,'r) im_state = 
  Insert_many.mk_insert_state

let im_step ~constants ~cmp ~store_ops : 'im -> ('im,'t) m = 
  (fun is -> Insert_many.insert_step (X.x_ps1 ~constants ~cmp ~store_ops) is)

let dest_im_finished: ('k,'v,'r)im_state -> ('r*('k*'v)list) option = 
  Insert_many.dest_i_finished
  (* no wf *)


(* leaf stream ---------------------------------------- *)

(** Given a reference to a B-tree root, construct a stream of leaves *)
let mk_ls_state : 'r -> ('k,'v,'r) ls_state = Leaf_stream.mk_ls_state

(** Step the leaf stream to the next leaf. If the leaf stream is
   finished (no more leaves), stepping will just return the leaf
   stream unchanged. So in the loop you need to check whether you have
   finished using [ls_is_finished]. FIXME here and elsewhere, staging *)
let ls_step ~constants ~cmp ~store_ops ls 
  : 't -> 't * ('k,'v,'r) ls_state res = 
  Leaf_stream.lss_step (X.x_ps1 ~constants ~cmp ~store_ops) ls

(** Return the (key,value) list at the current leaf in the stream. *)
let ls_dest_leaf ls = Leaf_stream.dest_LS_leaf ls

let ls_is_finished lss : bool = (lss |> Leaf_stream.lss_is_finished)


