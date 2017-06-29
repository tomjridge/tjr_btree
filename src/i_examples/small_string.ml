(** Strings with <= 256 bytes, used as keys in maps *)
open Test

module SS : sig
  type t [@@deriving bin_io, yojson]
  type ss = t
  val to_string: t -> string
  val of_string: string -> t
  val max_size: int
  val compare: t -> t -> int
end = struct
  open Bin_prot.Std
  type t = string [@@deriving bin_io, yojson]
  type ss = t
  let to_string x = x
  let max_size = 256
  let of_string x = (
    test(fun () -> assert (String.length x <= max_size));
    x)
  let compare: t -> t -> int = Pervasives.compare
end

type ss = SS.ss 


(*
module O = struct
  type ss = Small_string.ss
  module SS = Small_string  (* abbrev *)
end
*)
