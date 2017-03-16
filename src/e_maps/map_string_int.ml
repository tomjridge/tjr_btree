(* map from string (<=256 bytes) to int *)

(* open Ext_block_device *)

(* assumptions ---------------------------------------- *)

let key_size = 256 + 4  (* length *)

let value_size = 4  (* 32 bit ints *)

(* instantiate Btree.Simple.Make() ----------------------------------------- *)

module Make = functor (ST:Btree_api.Simple.STORE) -> struct
  module ST = ST
  module Simple = Btree_simple.Make(
    struct 

      module KV = struct

        type key = string  [@@deriving yojson] (* 16 char strings *)

        type value = int  [@@deriving yojson]

        let key_ord = String.compare

        let equal_value x y = (x:int) = y

      end (* KV *)

      let _ = (module KV : Btree.KEY_VALUE_TYPES)

      module ST=ST

      open KV
      let pp: (key,value) Btree_api.Pickle_params.t = Pickle.(
        {
          p_k = (fun k -> Examples.p_string_w_len k);
          u_k = (Examples.u_string_w_len);
          k_len = key_size;
          p_v = Examples.p_int;
          u_v = Examples.u_int;
          v_len = value_size;
        })

    end) (* Make *)

end
