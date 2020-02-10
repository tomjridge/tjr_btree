(** Int-like types with unboxed record wrappers, for documentation purposes *)

(* FIXME move to fs_shared *)

type 'int size_ = {
  size:'int
} [@@unboxed]

type size = int size_

type 'int offset_ = {
  off:'int
} [@@unboxed]

type offset = int offset_

type 'int len_ = {
  len:int
} [@@unboxed]

type len = int len_


type 'int index_ = {
  index:'int;
} [@@unboxed]

type index = int index_


type 'int id_ = {
  id:'int
} [@@unboxed]

type id = int id_
