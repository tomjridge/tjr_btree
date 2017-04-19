(* map from int to int *)

open Prelude

module KV = struct
  type key = int[@@deriving yojson]
  type value = int[@@deriving yojson]
  let key_ord (x:int) y = Pervasives.compare x y
  let equal_value : value -> value -> bool = (=)
end

open KV
open Pickle_params

let pp = Pickle.Examples.{
    p_k = p_int;
    u_k = u_int;
    k_len = 4;
    p_v = p_int;
    u_v = u_int;
    v_len = 4;
  }

