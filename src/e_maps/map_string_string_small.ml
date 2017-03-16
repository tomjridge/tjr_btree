(* map from string (<=256 bytes) to string (<=256 bytes) *)

(* open Ext_block_device *)


(* assumptions ---------------------------------------- *)

let key_size = 256 + 4  (* length *)

(* value = key *)

(* a type of short strings ---------------------------------------- *)

module Small_string : sig
  type t [@@deriving yojson]
  val to_string: t -> string
  val from_string: string -> t
end = struct
  type t = string [@@deriving yojson]
  let to_string x = x
  let from_string x = (
    assert (String.length x <= 256);
    x)
end

module SS = Small_string

module KV = struct
  open Small_string
  (* open Small_string *)
  type key = t  [@@deriving yojson] 
  type value = t  [@@deriving yojson]
  let key_ord x y = String.compare (to_string x) (to_string y)
  let equal_value x y = (to_string x = to_string y)
end (* KV *)

let _ = (module KV : Btree.KEY_VALUE_TYPES)

(* instantiate Btree.Simple.Make() ----------------------------------------- *)

module Make = functor (ST:Btree_api.Simple.STORE) -> struct
  module ST = ST
  module Simple = Btree_simple.Make(
    struct 
      module KV = KV
      module ST=ST
      open KV
      open Small_string
      let pp: (key,value) Btree_api.Pickle_params.t = Pickle.(
          let p_k = (fun k -> Examples.p_string_w_len (to_string k)) in
          let u_k = (Examples.u_string_w_len |> U.map from_string) in
          {
            p_k = p_k;
            u_k = u_k;
            k_len = key_size;
            p_v = p_k;
            u_v = u_k;
            v_len = key_size;
          })

    end) (* Make *)

end
