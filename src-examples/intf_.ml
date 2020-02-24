(** Input signature (types k and v, and related info) *)
module type S = sig
    type k                              [@@deriving bin_io]
    type v                              [@@deriving bin_io]
    val k_cmp : k -> k -> int
    val cs    : constants
      
    val debug_k_and_v_are_int: bool
  end

(** Flags:

- O_TRUNC, reinitialize B-tree root to point to an empty leaf; possibly also truncate the underlying file
- O_NOCACHE, do not use a write back cache (the underlying file may still need to be sync'ed tho)
*)
type flg = O_TRUNC | O_NOCACHE



(** Output signature (EX is "example") *)
module type EX = sig
  type k
  type v

  (** monadic type, typically lwt *)
  type t

  (** ls is "leaf stream" *)
  type ls

  (** bd is "B-tree descriptor" *)
  type bd

  (** By default, open_ will not truncate, and will use a cache (ie flgs is empty) *)
  val open_        : ?flgs:(flg list) -> string -> (bd, t) m
  val close        : bd:bd -> (unit, t) m

  val find         : bd:bd -> k:k -> (v option, t) m
  val insert       : bd:bd -> k:k -> v:v -> (unit, t) m
  val insert_many  : bd:bd -> k:k -> v:v -> kvs:(k * v) list -> ((k * v) list, t) m
  val insert_all   : bd:bd -> kvs:(k * v) list -> (unit, t) m
  val delete       : bd:bd -> k:k -> (unit, t) m

  val sync_to_disk : bd:bd -> (unit, t) m
  val flush_cache  : bd:bd -> (unit, t) m

  val ls_create    : bd:bd -> (ls, t) m
  val ls_step      : bd:bd -> ls:ls -> (ls option, t) m
  val ls_kvs       : bd:bd -> ls:ls -> (k * v) list
end




