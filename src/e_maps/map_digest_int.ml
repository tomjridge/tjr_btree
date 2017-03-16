(* map from string to int using Digest.t MD5 128bit hash of string *)

(* open Ext_block_device *)

(* assumptions ---------------------------------------- *)

let digest_size = 128/8 (* bytes *)

let key_size = digest_size

let value_size = 4  (* 32 bit ints *)

(* instantiate Btree.Simple.Make() ----------------------------------------- *)

module Make = functor (ST:Btree_api.Simple.STORE) -> struct

  module Simple = Btree_simple.Make(
    struct 

      module KV = struct

        (* we map string to digest using hash *)
        type pre_key = string

        type digest_t = string  [@@deriving yojson]

        type key = digest_t  [@@deriving yojson] (* 16 char strings *)

        type value = int  [@@deriving yojson]

        let key_ord = Digest.compare

        let equal_value x y = (x:int) = y

      end (* KV *)

      let _ = (module KV : Btree.KEY_VALUE_TYPES)

      module ST=ST

      open KV
      let pp: (key,value) Btree_api.Pickle_params.t = Pickle.(
        {
          p_k = (fun k -> Examples.p_string k);
          u_k = (Examples.u_string key_size);
          k_len = key_size;
          p_v = Examples.p_int;
          u_v = Examples.u_int;
          v_len = value_size;
        })

    end) (* Make *)

end

(* FIXME check result type *)
