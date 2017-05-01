(* digest ------------------------------------------------------------ *)

module Digest : sig 
  type t = private string [@@deriving yojson]
  type digest = t
  val size : int 
  val compare : t -> t -> int
  val of_string: string -> t
  val to_string: t -> string
end = struct
  type t = string [@@deriving yojson]
  type digest = t
  let size = 128/8 (* 128 bits/8 bits per byte gives size in bytes *)
  let compare = String.compare
  let of_string x = x
  let to_string x = x
end

module O = struct
  type digest = Digest.t
  module DIG = Digest
end
