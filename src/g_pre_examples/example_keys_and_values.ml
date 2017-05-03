(** Various common keys and values, including digests, small strings, and ints *)

open Prelude
open Btree_api

open Digest_.O
open Small_string.O

open Pickle

module PE = Pickle.Examples

module Int = struct
  include Int
  let size = 4
end


(* digest -> int ---------------------------------------------------- *)

let digest_int_pp: (DIG.t,Int.t) pp = (
    {
      p_k = (fun k -> k|>DIG.to_string|>PE.p_string);
      u_k = (PE.u_string DIG.size |> U.map DIG.of_string);
      k_len = DIG.size;
      p_v = Examples.p_int;
      u_v = Examples.u_int;
      v_len = Int.size;
    })



(* digest -> ss * int ----------------------------------------------- *)

(* map from string to int using DIG.t MD5 128bit hash of string,
   and storing the real string as part of the value *)

(* assume string has length <= 256 *)

(* equal_v is (=) *)

let digest_ss_x_int_pp: (DIG.t,SS.t*Int.t) pp = (
    let key_size = DIG.size in
    let value_size = (4+SS.max_size) + 4 in (* length+string, int *) 
    {
      p_k = (fun k -> k|>DIG.to_string|>PE.p_string);
      u_k = (PE.u_string key_size |> U.map DIG.of_string);
      k_len = key_size;
      p_v = (fun (s,i) -> 
          let s = SS.to_string s in PE.(p_pair (p_string_w_len s) (p_int i)));
      u_v = PE.(
          u_pair u_string_w_len (fun _ -> u_int) 
          |> U.map (fun (s,i) -> (SS.of_string s,i)));
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

let int_int_pp = PE.{
    p_k = p_int;
    u_k = u_int;
    k_len = 4;
    p_v = p_int;
    u_v = u_int;
    v_len = 4;
  }


(* ss -> int -------------------------------------------------------- *)

let ss_int_pp: (SS.t,int) pp = Pickle_params.(
    let key_size = SS.max_size + 4 in (* string + 4 bytes for length *)
    let value_size = 4  in (* 32 bit ints *)
    {
      p_k = (fun k -> k|>SS.to_string|>PE.p_string_w_len);
      u_k = (PE.u_string_w_len |> U.map SS.of_string);
      k_len = key_size;
      p_v = PE.p_int;
      u_v = PE.u_int;
      v_len = value_size;
    })


(* ss -> ss --------------------------------------------------------- *)

let ss_ss_pp: (SS.t,SS.t) pp = Pickle_params.(
    let key_size = SS.max_size + 4 in (* length *)
    let p_k = (fun k -> k|>SS.to_string|>PE.p_string_w_len) in
    let u_k = (PE.u_string_w_len |> U.map SS.of_string) in
    {
      p_k = p_k;
      u_k = u_k;
      k_len = key_size;
      p_v = p_k;
      u_v = u_k;
      v_len = key_size;
    })


(* FIXME can probably factor out above definitions to share common parts eg p_int_k etc *)
