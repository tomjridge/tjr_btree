(* implementations of disk interface *)

open Prelude
open Btree_api

module M = (World : MONAD with type 'a m = 'a World.m)

module Mk_block = functor (S:sig val sz: int end) -> struct
  module S = S
  type t = string
  type r = int [@@deriving yojson]
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

let _ = (module (Mk_block(struct let sz=1024 end)) : BLK_LIKE)


(* DISK impl backed by fd ---------------------------------------- *)

module Disk_backed_by_file = struct
  include M

  type fd = Unix.file_descr
  type t = fd World.r  (* NB a ref to an fd *)

  module BLK = Mk_block(struct let sz=4096 end)
  let _ = (module BLK : BLK_LIKE)

  open M
  let safely = Sem.safely

  let get_fd : t -> fd m = fun t -> World.get t

  let read : t -> BLK.r -> BLK.t m = (
    fun t r -> 
      safely __LOC__ (
        get_fd t
        |> bind Unix.(fun fd ->
            ignore (lseek fd (r * BLK.sz) SEEK_SET);
            let buf = Bytes.make BLK.sz (Char.chr 0) in 
            let n = read fd buf 0 BLK.sz in
            (* assert (n=BLK.sz); we allow the file to expand automatically, so no reason to read any bytes *)
            assert(n=0 || n=BLK.sz);
            return buf)))

  let write: t -> BLK.r -> BLK.t -> unit m = (
    fun t r buf -> 
      safely __LOC__ (
        get_fd t
        |> bind Unix.(fun fd ->
            ignore (lseek fd (r * BLK.sz) SEEK_SET);
            let n = single_write fd buf 0 BLK.sz in
            assert (n=BLK.sz);
            return ())))

  let disk_sync : t -> unit m = ExtUnixSpecific.(fun t -> 
      safely __LOC__ (
        get_fd t |> bind (fun fd -> ExtUnixSpecific.fsync fd; return ())))

  let from_file ~fn ~create ~init = Unix.(
      let flgs = 
        [O_RDWR] 
        @ (if create then [O_CREAT] else [])
        @ (if init then [O_TRUNC] else [])
      in
      ((openfile fn flgs 0o640) : fd)
    )
  

end

let _ = (module Disk_backed_by_file : Btree_api.DISK)



