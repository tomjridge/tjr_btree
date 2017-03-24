(* a generic functional store ---------------------------------------- *)

open Btree_util

type 'a ref_ = int

(* universal type *)
type univ

type t = { map: univ Map_int.t; free: int }

let mk_ref: 'a. 'a -> t -> (t * 'a ref_) = (
  fun x t -> (
      let r = t.free in
      let free = r+1 in
      let map = Map_int.add r (Obj.magic x) t.map in
      ({map;free},r)))

let set: 'a. 'a ref_ -> 'a -> t -> t = (
  fun r x t -> (
      let map = Map_int.add r (Obj.magic x) t.map in
      {t with map=map}))

let get: 'a. 'a ref_ -> t -> 'a = (
  fun r t ->
    Obj.magic(Map_int.find r t.map))

