type blk_index = int
type offset = int
module type BUFFER =
  sig type t val length : t -> int val create : int -> t end
module type DISK_T =
  sig
    module Buff : BUFFER
    type store
    type 'a m = ('a, store) Prelude.Sem.m
    type block
    type blk_id = int
    val block_size : int
    val write_buff : Buff.t -> offset -> blk_id m
    val read_buff : Buff.t -> offset -> blk_id -> unit m
  end
module type BTREE =
  sig
    module Disk : DISK_T
    type ref_t = int
    val empty_btree : unit -> ref_t Disk.m
    val insert : blk_index -> Disk.blk_id -> ref_t -> ref_t Disk.m
    val find : ref_t -> blk_index -> Disk.blk_id option Disk.m
  end
module type S =
  sig
    module Buff : BUFFER
    module Disk :
      sig
        module Buff :
          sig type t = Buff.t val length : t -> int val create : int -> t end
        type store
        type 'a m = ('a, store) Prelude.Sem.m
        type block
        type blk_id = int
        val block_size : int
        val write_buff : Buff.t -> offset -> blk_id m
        val read_buff : Buff.t -> offset -> blk_id -> unit m
      end
    module Btree :
      sig
        module Disk :
          sig
            module Buff :
              sig
                type t = Buff.t
                val length : t -> int
                val create : int -> t
              end
            type store = Disk.store
            type 'a m = ('a, store) Prelude.Sem.m
            type block = Disk.block
            type blk_id = int
            val block_size : int
            val write_buff : Buff.t -> offset -> blk_id m
            val read_buff : Buff.t -> offset -> blk_id -> unit m
          end
        type ref_t = int
        val empty_btree : unit -> ref_t Disk.m
        val insert : blk_index -> Disk.blk_id -> ref_t -> ref_t Disk.m
        val find : ref_t -> blk_index -> Disk.blk_id option Disk.m
      end
  end
module Make :
  functor (S : S) ->
    sig
      module S :
        sig
          module Buff :
            sig
              type t = S.Buff.t
              val length : t -> int
              val create : int -> t
            end
          module Disk :
            sig
              module Buff :
                sig
                  type t = Buff.t
                  val length : t -> int
                  val create : int -> t
                end
              type store = S.Disk.store
              type 'a m = ('a, store) Prelude.Sem.m
              type block = S.Disk.block
              type blk_id = int
              val block_size : int
              val write_buff : Buff.t -> offset -> blk_id m
              val read_buff : Buff.t -> offset -> blk_id -> unit m
            end
          module Btree :
            sig
              module Disk :
                sig
                  module Buff :
                    sig
                      type t = Buff.t
                      val length : t -> int
                      val create : int -> t
                    end
                  type store = Disk.store
                  type 'a m = ('a, store) Prelude.Sem.m
                  type block = Disk.block
                  type blk_id = int
                  val block_size : int
                  val write_buff : Buff.t -> offset -> blk_id m
                  val read_buff : Buff.t -> offset -> blk_id -> unit m
                end
              type ref_t = int
              val empty_btree : unit -> ref_t Disk.m
              val insert : blk_index -> Disk.blk_id -> ref_t -> ref_t Disk.m
              val find : ref_t -> blk_index -> Disk.blk_id option Disk.m
            end
        end
      val meta_key : int
      val write_buff : S.Btree.Disk.Buff.t -> S.Btree.ref_t S.Btree.Disk.m
      val read_buff : S.Btree.ref_t -> S.Btree.Disk.Buff.t S.Btree.Disk.m
    end
