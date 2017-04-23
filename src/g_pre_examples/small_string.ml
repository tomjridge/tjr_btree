(* a type of short strings ---------------------------------------- *)

module Small_string : sig
  type t [@@deriving yojson]
  val to_string: t -> string
  val of_string: string -> t
  val max_size: int
  val compare: t -> t -> int
end = struct
  type t = string [@@deriving yojson]
  let to_string x = x
  let max_size = 256
  let of_string x = (
    assert (String.length x <= max_size);
    x)
  let compare: t -> t -> int = Pervasives.compare
end

module SS_ = Small_string
