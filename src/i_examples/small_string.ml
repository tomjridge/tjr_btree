(** Strings with <= 256 bytes, used as keys in maps *)
open Test

module SS : sig
  type ss [@@deriving bin_io, yojson]
  type t = ss
  val max_length: int
  val to_string: t -> string
  val of_string: string -> t
  val compare: t -> t -> int
end = struct
  open Bin_prot.Std
  type ss = string [@@deriving bin_io, yojson]
  type t = ss
  let max_length = 256 
  let to_string x = x
  let of_string x = (
    test(fun () -> assert (String.length x <= max_length));
    x)
  let compare: t -> t -> int = Pervasives.compare
end

include SS



(*
module O = struct
  type ss = Small_string.ss
  module SS = Small_string  (* abbrev *)
end
*)
