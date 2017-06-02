(** Basic block implementation. *)

type blk_sz = int  (* in bytes *)

(** Basic implementation of a block. Blocks are represented by
   strings. Blocks have a particular length, although we do not
   capture this in the type. *)
module BlkN : sig
  type blk
  type blk_id = int
  val of_string: blk_sz -> string -> blk
  val to_string: blk -> string
  val compare_blk_id: blk_id -> blk_id -> int 
end = struct
  type blk = string
  type blk_id = int
  let of_string: blk_sz -> string -> blk = (
    fun sz s ->
      Test.test (fun _ -> assert (String.length s <= sz)); 
      s ^ (String.make (sz - String.length s) (Char.chr 0))
  )
  let to_string: blk -> string = fun x -> x
  let compare_blk_id = Int_.compare 
end

module Blk4096 = struct
  include BlkN
  (** Default block size. Clearly this should match the block size of
      the underlying storage. *)
  let blk_sz = 4096
  let of_string = of_string blk_sz
end

(* default *)
include BlkN

