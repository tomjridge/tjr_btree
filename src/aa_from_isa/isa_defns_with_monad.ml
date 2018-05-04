(** Combine the isabelle defns with the monad *)

open Tjr_monad
open Isa_btree_defns

let make (type t) ~(monad_ops: t Monad.monad_ops) = 
  let module Monad = struct
    type ('a,'t) mm = ('a,t) Monad.m  (* FIXME this is a bit funny, because the 't is not really playing a role on the isa side *)
    let return x : ('a,t) mm = monad_ops.return x
    let bind (ab: 'a -> ('b,t) mm) (a:('a,t)mm) : ('b,t) mm = monad_ops.bind a ab
    let dummy = ()
    let fmap f a = bind (fun a -> return (f a)) a
  end
  in
  let module M = Isa_btree_defns.Make(Monad) in
  M.store_ops_to_pre_map_ops


let _ = make
