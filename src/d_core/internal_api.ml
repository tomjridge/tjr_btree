(* deprecated, use btree_api.ml ---------------------------------------- *)

open Prelude

(* FIXME get rid of this/tidy it up *)


(* oft used instance *)
module Int_key = struct
  type key = int
  let key_ord: key -> key -> int = fun k1 (k2:int) -> Pervasives.compare k1 k2
end







(* FIXME remove *)
module Types = struct
  type blk = string [@@deriving yojson]
  type blk_id = int [@@deriving yojson]
end

(* FIXME remove *)
module type BUFFER = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
  val length: t -> int
end

(* default implementation *)
module Buffer = struct
  type t = string
  let of_string x = x
  let to_string x = x
  let length x = String.length x
end

let _ = (module Buffer: BUFFER)

(* FIXME remove in favour of btree_api *)
(* FIXME do we want pages to have a similar structure ie with sz? or
   perhaps carve out some subsets of string with fixed sizes? pages
   are blocks without ids *)
module type BLOCK = (sig
  type t = Types.blk
  type id = Types.blk_id
  val sz: int
  val string_to_blk: string -> (t,string) result
  val empty: unit -> t
end)


module Mk_block = functor (S:sig val block_size: int end) -> struct
  module S = S
  type t = Types.blk
  type id = Types.blk_id
  let sz = S.block_size
  let string_to_blk: string -> (t,string) result = (
    fun x -> 
      let l = String.length x in
      let c = Pervasives.compare l (S.block_size) in
      match c with
      | 0 -> Ok x
      | _ when c < 0 -> Ok (x^(String.make (S.block_size - l) (Char.chr 0)))
      | _ -> Error (__LOC__ ^ "string too large: " ^ x)
  )
  let empty: unit -> t = fun () -> String.make S.block_size (Char.chr 0)
end

let _ = (module (Mk_block(struct let block_size=1024 end)) : BLOCK)


(* what is typically provided by the file system; used to provide the
   store interface *)
module type BLOCK_DEVICE = sig
  module Block : BLOCK
  type t (* type of block device *)
  type r = Block.id (* blk references *)
  type blk = Block.t
  type 'a m = ('a,t) Sem.m

  (* val block_size: t -> int *)
  val read: r -> blk m
  val write: r -> blk -> unit m
  val sync: unit -> unit m  
end







(* simple interface ---------------------------------------- *)

(* see btree_simple_internal *)

(*
module Pickle_params = Btree_api.Pickle_params

module Simple = struct
  type page_ref = int[@@deriving yojson]
  type page = string (* ie immutable bytes *)

  module type STORE = sig (* FIXME STORE *)
    include STORE with type page_ref = page_ref
                   and type page = page 
    val page_size : int (* bytes per page; needed to compute constants *)
  end

  module type S = sig
    module KV: KEY_VALUE
    module ST: STORE
    open KV
    val pp: (key,value) Pickle_params.t 
  end (* S *)
end


*)
