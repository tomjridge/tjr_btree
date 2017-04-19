(* map from string (<=256 bytes) to string (<=256 bytes) *)

open Prelude
open Map_prelude


let key_size = Small_string.max_size + 4  (* length *)

module KV = struct
  open SS_
  type key = SS_.t  [@@deriving yojson] 
  type value = SS_.t  [@@deriving yojson]
  let key_ord = SS_.compare
  let equal_value (x:value) y = (x=y)
end

open KV
module EX_ = Pickle.Examples

let pp: (key,value) Pickle_params.t = Pickle_params.(
    let p_k = (fun k -> k|>SS_.to_string|>EX_.p_string_w_len) in
    let u_k = (EX_.u_string_w_len |> Pickle.U.map SS_.of_string) in
    {
      p_k = p_k;
      u_k = u_k;
      k_len = key_size;
      p_v = p_k;
      u_v = u_k;
      v_len = key_size;
    })
