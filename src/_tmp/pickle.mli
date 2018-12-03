module Basic_marshalling :
  sig
    val int32_to_bytes : int32 -> char list
    val bytes_to_int32 : char list -> int32
  end
type pickle_target_t = string
type ptt = pickle_target_t
type pickle_error = string
exception Pickle_exception of string
module P :
  sig
    type m
    val ret : unit -> m
    val bind : (unit -> m) -> m -> m
    val run : ptt -> m -> ptt * pickle_error option
    val run_w_exception : ptt -> m -> ptt
    val write_bytes : char list -> m
  end
module U :
  sig
    type 'a m = ptt -> ptt * ('a, pickle_error) result
    val bind : ('a -> 'b m) -> 'a m -> 'b m
    val ret : 'a -> 'a m
    val run : ptt -> 'a m -> ptt * ('a, pickle_error) result
    val run_w_exception : ptt -> 'a m -> ptt * 'a
    val read_bytes : int -> char list m
    val map : ('a -> 'b) -> 'a m -> 'b m
  end
module Examples :
  sig
    val p_pair : P.m -> P.m -> P.m
    val u_pair : 'a U.m -> ('a -> 'b U.m) -> ('a * 'b) U.m
    val p_int32 : int32 -> P.m
    val u_int32 : int32 U.m
    val p_int : int -> P.m
    val u_int : int U.m
    val p_string : string -> P.m
    val u_string : int -> string U.m
    val p_string_w_len : string -> P.m
    val u_string_w_len : string U.m
    val p_list : ('a -> P.m) -> 'a list -> P.m
    val u_list : 'a U.m -> 'a list U.m
  end
module String_int :
  sig
    module KV :
      sig
        type k = string
        val k_to_yojson : k -> Yojson.Safe.json
        val k_of_yojson :
          Yojson.Safe.json -> k Ppx_deriving_yojson_runtime.error_or
        type v = int
        val v_to_yojson : v -> Yojson.Safe.json
        val v_of_yojson :
          Yojson.Safe.json -> v Ppx_deriving_yojson_runtime.error_or
        val key_ord : k -> k -> int
        val equal_value : v -> v -> bool
      end
    val key_size : int
    val p_key : string -> P.m
    val u_k : KV.k U.m
    val p_ks : KV.k list -> P.m
    val u_ks : KV.k list U.m
  end
