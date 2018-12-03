module EX_ = Pickle.Examples
module Digest :
  sig
    type t = Digest_.Digest.t
    val to_yojson : t -> Yojson.Safe.json
    val of_yojson :
      Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
    val sz : int
    val of_string : string -> t
    val to_string : t -> string
    val size : int
    val compare : Digest.t -> Digest.t -> int
  end
module Int :
  sig type t = int val size : int val compare : int -> int -> int end
val digest_int_pp : (Digest.t, Int.t) Prelude.Pickle_params.t
val digest_ss_x_int_pp :
  (Digest.t, Small_string.SS_.t * Int.t) Prelude.Pickle_params.t
val int_int_pp : (int, int) Prelude.Pickle_params.t
val ss_int_pp : (Small_string.SS_.t, int) Prelude.Pickle_params.t
val ss_ss_pp :
  (Small_string.SS_.t, Small_string.SS_.t) Prelude.Pickle_params.t
