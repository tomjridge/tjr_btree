(** Strings with <= 256 bytes, used as keys in maps *)
module SS : sig
  type ss [@@deriving bin_io, yojson]
  val max_length: int
  val to_string: ss -> string
  val of_string: string -> ss
  val compare: ss -> ss -> int
end = struct
  open Bin_prot.Std
  type ss = string [@@deriving bin_io, yojson]
  let max_length = 256 
  let to_string x = x
  let of_string x = (
    Test.test(fun () -> assert (String.length x <= max_length));
    x)
  let compare: ss -> ss -> int = Pervasives.compare
end

include SS

