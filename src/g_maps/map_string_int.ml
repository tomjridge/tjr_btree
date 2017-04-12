(* map from string (<=256 bytes) to int *)

open Prelude
open Map_prelude

(* assumptions ---------------------------------------- *)

let key_size = 256 + 4  (* string + 4 bytes for length *)

let value_size = 4  (* 32 bit ints *)


(* KV ------------------------------------------------------------ *)

module KV = struct
    type key = Small_string.t  [@@deriving yojson] 
    type value = int  [@@deriving yojson]
    let key_ord = String.compare
    let equal_value x y = (x:int) = y
  end (* KV *)

open KV
module EX_ = Pickle.Examples

let pp: (key,value) Pickle_params.t = Pickle_params.(
    {
      p_k = (fun k -> k|>SS_.to_string|>EX_.p_string_w_len);
      u_k = (EX_.u_string_w_len |> Pickle.U.map SS_.of_string);
      k_len = key_size;
      p_v = EX_.p_int;
      u_v = EX_.u_int;
      v_len = value_size;
    })

