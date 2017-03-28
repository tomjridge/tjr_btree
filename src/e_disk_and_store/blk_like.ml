(* standard impl of BLK_LIKE ---------------------------------------- *)

open Prelude
open Btree_api

module Make = functor (S:sig val sz: int end) -> struct
  type t = string
  type r = Btree_api.ptr [@@deriving yojson]
  let sz = S.sz
  let of_string: string -> (t,string) result = (
    fun x -> 
      let l = String.length x in
      let c = Pervasives.compare l sz in
      match c with
      | 0 -> Ok x
      | _ when c < 0 -> Ok (x^(String.make (sz - l) (Char.chr 0)))
      | _ -> Error (__LOC__ ^ "string too large: " ^ x)
  )
  let empty: unit -> t = fun () -> String.make sz (Char.chr 0)
end

let _ = (module (Make(struct let sz=1024 end)) : BLK_LIKE)

