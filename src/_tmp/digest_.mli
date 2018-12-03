module Digest :
  sig
    type t = private string
    val to_yojson : t -> Yojson.Safe.json
    val of_yojson :
      Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
    val sz : int
    val compare : t -> t -> int
    val of_string : string -> t
    val to_string : t -> string
  end
