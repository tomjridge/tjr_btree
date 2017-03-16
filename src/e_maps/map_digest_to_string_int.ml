(* map from string to int using Digest.t MD5 128bit hash of string,
   and storing the real string as part of the value *)

(* open Ext_block_device *)

(* assumptions ---------------------------------------- *)

let digest_size = 128/8 (* bytes *)

let key_size = digest_size

let value_size = (4+256) + 4  (* length+string, int *)

(* instantiate Btree.Simple.Make() ----------------------------------------- *)

module Make = functor (ST:Btree_api.Simple.STORE) -> struct

  module Simple = Btree_simple.Make(
    struct 

      module KV = struct

        (* we map string to digest using hash *)
        type pre_key = string

        type digest_t = string  [@@deriving yojson]

        type key = digest_t  [@@deriving yojson] (* 16 char strings *)

        type value = string * int  [@@deriving yojson]

        let key_ord = Digest.compare

        let equal_value x y = (x:value) = y

      end (* KV *)

      let _ = (module KV : Btree.KEY_VALUE_TYPES)

      module ST=ST

      open KV
      let pp: (key,value) Btree_api.Pickle_params.t = Pickle.(
        {
          p_k = (fun k -> Examples.p_string k);
          u_k = (Examples.u_string key_size);
          k_len = key_size;
          p_v = (fun (s,i) -> Examples.(p_pair (p_string_w_len s) (p_int i)));
          u_v = Examples.(u_pair u_string_w_len (fun _ -> u_int));
          v_len = value_size;
        })

    end) (* Make *)

end

(* FIXME check result type *)
