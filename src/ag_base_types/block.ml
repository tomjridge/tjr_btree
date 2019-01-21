(** Basic block interface. *)

include Tjr_fs_shared.Block_ops






(* open Tjr_fs_shared *)

(*
module type BLK_TYPES = sig
  type blk
  type blk_id = int 
end
*)

(* type blk_sz = int  (\* in bytes *\) *)


(*
(** Basic implementation of a block. Blocks are represented by
   strings. Blocks have a particular length, although we do not
   capture this in the type. *)
(* FIXME use bytes? what do we assume about mutability? *)
module BlkN : sig
  include BLK_TYPES
  val of_string: blk_sz -> string -> blk
  val to_string: blk -> string
  val compare_blk_id: blk_id -> blk_id -> int 
  val of_bytes: blk_sz -> bytes -> blk
  val to_bytes: blk -> bytes
end = struct
  type blk = string
  type blk_id = int 
  let of_string: blk_sz -> string -> blk = (
    fun sz s ->
      Test.test (fun _ -> assert (String.length s <= sz)); 
      s ^ (String.make (sz - String.length s) (Char.chr 0))
  )
  let to_string: blk -> string = fun x -> x
  let compare_blk_id = Tjr_int.compare 
  let of_bytes sz bs = bs |> Bytes.to_string |> of_string sz
  let to_bytes blk = blk |> to_string |> Bytes.of_string
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

*)
