(* map from string to int using Digest.t MD5 128bit hash of string,
   and storing the real string as part of the value *)

(* assume string has length <= 256 *)

open Prelude
open Map_prelude

let key_size = Digest.sz

let value_size = (4+Small_string.max_size) + 4  (* length+string, int *)

module KV = struct
  type key = Digest.t  [@@deriving yojson] 
  type value = Small_string.t * int  [@@deriving yojson]
  let key_ord = Digest.compare
  let equal_value x y = (x:value) = y
end (* KV *)

open KV

module EX_ = Pickle.Examples

let pp: (key,value) Pickle_params.t = Pickle_params.(
    {
      p_k = (fun k -> k|>Digest.to_string|>EX_.p_string);
      u_k = (EX_.u_string key_size |> Pickle.U.map Digest.of_string);
      k_len = key_size;
      p_v = (fun (s,i) -> 
          let s = SS_.to_string s in EX_.(p_pair (p_string_w_len s) (p_int i)));
      u_v = EX_.(
          u_pair u_string_w_len (fun _ -> u_int) 
          |> Pickle.U.map (fun (s,i) -> (SS_.of_string s,i)));
      v_len = value_size;
    })

