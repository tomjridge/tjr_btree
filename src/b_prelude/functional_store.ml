(* a generic functional store ---------------------------------------- *)

module Int = struct
  type t = int 
  let compare: t -> t -> int = Pervasives.compare 
end

module Map_int = Map.Make(Int)

type 'a r = int  (* ref *)

(* universal type *)
type univ

type t = { map: univ Map_int.t; free: int }

let mk_ref: 'a. 'a -> t -> (t * 'a r) = (
  fun x t -> (
      let r = t.free in
      let free = r+1 in
      let map = Map_int.add r (Obj.magic x) t.map in
      ({map;free},r)))

let set: 'a. 'a r -> 'a -> t -> t = (
  fun r x t -> (
      let map = Map_int.add r (Obj.magic x) t.map in
      {t with map=map}))

let get: 'a. 'a r -> t -> 'a = (
  fun r t ->
    Obj.magic(Map_int.find r t.map))

