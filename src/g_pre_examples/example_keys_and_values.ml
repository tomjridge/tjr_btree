open Prelude
open Btree_api

open Digest
open Small_string

open Pickle
open Pickle_params

module EX_ = Pickle.Examples

module Digest = struct
  include Digest_.Digest
  let size = sz
  let compare = Digest.compare  
end

module Int = struct
  include Int
  let size = 4
  let compare (x:int) y = Pervasives.compare x y
end



(* digest -> int ---------------------------------------------------- *)

let digest_int_pp: (Digest.t,Int.t) Pickle_params.t = Pickle.(
    {
      p_k = (fun k -> k|>Digest.to_string|>EX_.p_string);
      u_k = U.(EX_.u_string Digest.size |> map Digest.of_string);
      k_len = Digest.size;
      p_v = Examples.p_int;
      u_v = Examples.u_int;
      v_len = Int.size;
    })



(* digest -> ss * int ----------------------------------------------- *)

(* map from string to int using Digest.t MD5 128bit hash of string,
   and storing the real string as part of the value *)

(* assume string has length <= 256 *)

(* equal_v is (=) *)

let digest_ss_x_int_pp: (Digest.t,SS_.t*Int.t) Pickle_params.t = Pickle.(
    let key_size = Digest.sz in
    let value_size = (4+Small_string.max_size) + 4 in (* length+string, int *) 
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
    }
  )


(* int -> int ------------------------------------------------------- *)


(*
let int_int_kv_ops = {
  compare_k=(Pervasives.compare : int -> int -> int);
  equal_v=(fun x y -> (x:int) = y);
}
*)

let int_int_pp = EX_.{
    p_k = p_int;
    u_k = u_int;
    k_len = 4;
    p_v = p_int;
    u_v = u_int;
    v_len = 4;
  }


(* ss -> int -------------------------------------------------------- *)

let ss_int_pp: (SS_.t,int) Pickle_params.t = Pickle_params.(
    let key_size = Small_string.max_size + 4 in (* string + 4 bytes for length *)
    let value_size = 4  in (* 32 bit ints *)
    {
      p_k = (fun k -> k|>SS_.to_string|>EX_.p_string_w_len);
      u_k = (EX_.u_string_w_len |> Pickle.U.map SS_.of_string);
      k_len = key_size;
      p_v = EX_.p_int;
      u_v = EX_.u_int;
      v_len = value_size;
    })


(* ss -> ss --------------------------------------------------------- *)

let ss_ss_pp: (SS_.t,SS_.t) Pickle_params.t = Pickle_params.(
    let key_size = Small_string.max_size + 4 in (* length *)
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


(* FIXME can probably factor out above definitions to share common parts eg p_int_k etc *)
