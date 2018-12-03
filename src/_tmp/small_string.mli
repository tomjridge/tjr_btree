module Small_string :
  sig
    type t
    val to_yojson : t -> Yojson.Safe.json
    val of_yojson :
      Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
    val to_string : t -> string
    val of_string : string -> t
    val max_size : int
    val compare : t -> t -> int
  end
module SS_ = Small_string
