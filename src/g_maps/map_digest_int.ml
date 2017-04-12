(* map from string to int using Digest.t MD5 128bit hash of string *)

open Prelude
open Map_prelude

open Digest

module D_ = Digest

(* k v ------------------------------------------------------------ *)

module KV = struct
    type key = Digest.t  [@@deriving yojson] (* 16 char strings *)
    let key_size = Digest.sz
    let key_ord = Digest.compare

    type value = int  [@@deriving yojson]
    let value_size = 4  (* 32 bit ints *)
    let equal_value x y = (x:int) = y
  end

(* pickling ------------------------------------------------------------ *)

open KV 
open Btree_api

let pp: (key,value) Pickle_params.t = Pickle.(
    {
      p_k = (fun k -> k|>D_.to_string|>Pickle.Examples.p_string);
      u_k = U.(Pickle.Examples.u_string key_size |> map D_.of_string);
      k_len = key_size;
      p_v = Examples.p_int;
      u_v = Examples.u_int;
      v_len = value_size;
    })

