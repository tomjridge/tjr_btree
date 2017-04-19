(* map from string to int using Digest.t MD5 128bit hash of string *)

open Prelude
open Btree_api
open Map_prelude

open Digest

module D_ = Digest

module KV = struct
  type key = Digest.t  [@@deriving yojson] (* 16 char strings *)
  let key_size = Digest.sz
  let key_ord = Digest.compare

  type value = int  [@@deriving yojson]
  let value_size = 4  (* 32 bit ints *)
  let equal_value x y = (x:int) = y
end

open KV 

let pp: (key,value) Pickle_params.t = Pickle.(
    {
      p_k = (fun k -> k|>D_.to_string|>Pickle.Examples.p_string);
      u_k = U.(Pickle.Examples.u_string key_size |> map D_.of_string);
      k_len = key_size;
      p_v = Examples.p_int;
      u_v = Examples.u_int;
      v_len = value_size;
    })

