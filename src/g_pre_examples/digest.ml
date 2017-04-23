(* digest ------------------------------------------------------------ *)

module Digest : sig 
    type t = private string [@@deriving yojson]
    val sz : int 
    val compare : t -> t -> int
    val of_string: string -> t
    val to_string: t -> string
  end = struct
    type t = string [@@deriving yojson]
    let sz = 128/8 (* 128 bits/8 bits per byte gives size in bytes *)
    let compare = String.compare
    let of_string x = x
    let to_string x = x
end
