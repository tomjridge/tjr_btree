open Prelude

(** Basic implementation of a block. Blocks are represented by
   strings. Blocks have a particular length, although we do not
   capture this in the type. *)
module Default_block : sig
  type t
  type r = int
  type sz = int  (* of a block, in bytes *)
  val of_string: sz -> string -> t
  val to_string: t -> string
  val compare_r: r -> r -> int 
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
  let compare_r = Int_.compare 
end

module BLK = Default_block

(** Default filename for testing. *)
let fn = "./btree.store"

(** Default block size. Clearly this should match the block size of
   the underlying storage. *)
let blk_sz = 4096 (* page/block size *)
