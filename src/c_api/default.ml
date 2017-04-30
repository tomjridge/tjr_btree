open Prelude

module Default_block : sig
  type t
  type r = int
  type sz = int  (* of a block, in bytes *)
  val of_string: sz -> string -> t
  val to_string: t -> string
end = struct
  type t = string
  type r = int
  type sz = int 
  let of_string: sz -> string -> t = (
    fun sz s ->
      test (fun _ -> assert (String.length s <= sz)); 
      s ^ (String.make (sz - String.length s) (Char.chr 0))
  )
  let to_string: t -> string = fun x -> x
end

module BLK = Default_block

let default_filename = "./btree.store"

let default_size = 4096 (* page/block size *)
