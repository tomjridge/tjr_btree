(* store passing with error *)

module Lens = Lens.Lens


module Sem = State_error_monad.Sem

module type CONSTANTS = sig
  val max_leaf_size : int
  val max_node_keys : int
  val min_leaf_size : int
  val min_node_keys : int
end



(* we require that the store makes errors explicit so we can
   explicitly handle them (ie maintain invariants); if we don't have
   any resources, then an exception just unwinds the stack (so is
   safe); otherwise we need to use exceptions very carefully, in which
   case we might as well be explicit *)
module type STORE = sig
  type page
  type store
  type page_ref [@@deriving yojson]

  type 'a m = ('a,store) Sem.m

  val free: page_ref list -> unit m
  val alloc: page -> page_ref m
  val dest_Store: store -> page_ref -> page
  val page_ref_to_page: page_ref -> page m
end


module type KEY_VALUE = Btree_api.KEY_VALUE

(* oft used instance *)
module Int_key = struct
  type key = int
  let key_ord: key -> key -> int = fun k1 (k2:int) -> Pervasives.compare k1 k2
end


(* a "raw" api, a layer over the stuff from isabelle -------------------- *)

(* like a map, but pointers are explicit *)
module type RAW_MAP = sig
  module KV : KEY_VALUE
  module ST : STORE
  type bt_ptr = ST.page_ref

  (* bt_ptr changes, as does store *)
  type 'a m = ('a,ST.store * bt_ptr) Sem.m

  open KV
  val empty: unit -> (bt_ptr,ST.store) Sem.m
  val insert: key -> value -> unit m
  val insert_many: key -> value -> (key*value) list -> unit m
  val find: key -> value option m
  val delete: key -> unit m

end


(* leaf stream ---------------------------------------- *)

(* this is a useful operation to support, but not needed always *)
module type LEAF_STREAM = sig
  module ST : STORE
  module KV : KEY_VALUE
  open KV

  type t  (* pointer to somewhere in btree *)
  type 'a m = ('a,ST.store * t) Sem.m

  val mk: ST.page_ref -> (t,ST.store) Sem.m
  val step: unit -> bool m  (* true if we have stepped *)
  val get_kvs: unit -> (key*value) list m
end





module Types = struct
  type blk = string [@@deriving yojson]
  type blk_id = int [@@deriving yojson]
end

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

(* see Btree_simple *)

module Pickle_params = struct
  open Pickle
  type ('k,'v) t = {
    p_k: 'k -> P.m;
    u_k: 'k U.m;
    k_len: int;
    p_v: 'v -> P.m;
    u_v: 'v U.m;
    v_len: int      
  }
end  

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


