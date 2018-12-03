(** Configuration type for [tjr_btree] *)


(* FIXME perhaps this should be wrt some fixed kv types? *)

type blk_dev_on_file = {
  blk_sz:int;
  filename:string
}

(* type blk_dev = On_file of blk_dev_on_file *)
  
type int_or_string = Int | String

(* assume page_ref=int *)

type key_value =
  | Int_int (* : (int,int) marshalling_params *)
  | String_string (* : (string,string) marshalling_params *)


(* FIXME use atd? marshalling params are really fixed by the kv types *)  
  
(** NOTE constants can be computed from marshalling_params and blk_sz *)
type t = {
  blk_dev: blk_dev_on_file;
  key_value: key_value
  (* marshalling_params: marshalling_params; *)
}

