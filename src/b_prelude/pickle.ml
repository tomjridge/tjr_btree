(* marshalling (low-level), followed by pickling *)

(* various marshalling stuff ---------------------------------------- *)


module Basic_marshalling = struct

  (* convert int to bytes *)

  let int32_to_bytes = Int32.(
      fun i0 -> 
        let arr = Array.make 4 (Char.chr 0) in
        for j = 0 to 3 do 
          let off = j in
          let c = (shift_right i0 (8*off)) |> logand (of_int 255) in
          arr.(off) <- (c|>to_int|>Char.chr)
        done;
        [arr.(0);arr.(1);arr.(2);arr.(3)])

  (* assumes bs length 4 *)
  let bytes_to_int32 = Int32.(fun bs -> 
      assert (List.length bs = 4);
      let arr = Array.of_list bs in
      let i = ref (Int32.of_int 0) in
      for j = 0 to 3 do
        let off = j in
        let c = shift_left (arr.(off)|>Char.code|>Int32.of_int) (8*off) in
        i:=(logor !i c)
      done;
      !i)

  let _ = Test.test (fun _ ->
      let f i = (
        assert (i |> int32_to_bytes |> bytes_to_int32 = i);
        let j = i |> int32_to_bytes in
        assert (j |> bytes_to_int32 |> int32_to_bytes = j);
        ())
      in
      List.iter f [Int32.of_int 0;Int32.of_int 1;Int32.of_int (-1);Int32.max_int;Int32.min_int])




end




(* various pickle/parsing routines ---------------------------------------- *)

(* FIXME move to tjr_lib *)

(* 

what is the contract? that if you pickle, it affects a range, and
unpickling starting at the beginning of the range consumes the range
(so that next pickler can work ok) and returns the same object you
pickled

*)

type pickle_target_t = string
type ptt = pickle_target_t

type pickle_error = string

exception Pickle_exception of string

  (* FIXME change this to buffer? yes; or even rev byte list? or
     bytes? what are we going to convert to eventually? probably bytes
     or string, so bytes is the obvious choice except that bytes is
     limited size; so use buffer; buffer append char is reasonably
     efficient; perhaps create with suitable 4096 byte size *)

(* pickle *)
module P : sig
  type m  (* = ptt -> ptt * pickle_error option *)
  val ret: unit -> m
  val bind: (unit -> m) -> m -> m
  val run:  ptt -> m -> ptt * pickle_error option
  val run_w_exception:  ptt -> m -> ptt
  val write_bytes: char list -> m
end = struct
  type m = ptt -> ptt * pickle_error option
  let ret () = (fun s -> (s,None))
  let  bind: (unit -> m) -> m -> m = (
    fun f m -> (
        fun s -> 
          match m s with
          | (s',None) -> (f () s')
          | (s',Some e) -> (s',Some e))
  )
  let run = (fun s m -> m s)
  let run_w_exception ptt m = m |> run ptt |> (fun (s',e) ->
      match e with 
      | None -> s'
      | Some e -> raise (Pickle_exception e))
  let write_bytes bs = (fun s -> (s^(BatString.implode bs),None))
end

module U : sig
  type 'a m = ptt -> ptt * ('a,pickle_error) result
  val bind: ('a -> 'b m) -> ('a m) -> ('b m)
  val ret: 'a -> 'a m
  val run: ptt -> 'a m -> ptt * ('a,pickle_error) result
  val run_w_exception: ptt -> 'a m -> ptt * 'a
  val read_bytes: int -> char list m 
  val map: ('a -> 'b) -> ('a m) -> ('b m)
end = struct 

  type 'a m = ptt -> ptt * ('a,pickle_error) result

  let bind f m = (fun s ->
      match m s with
      | (s',Ok x) -> (f x s')
      | (s',Error e) -> (s',Error e)
  )
  let ret x = (fun s -> (s,Ok x))
  let run s m = m s
  let run_w_exception s m = m |>run s|>(fun (s',e) ->
      match e with 
      | Ok(x) -> (s',x)
      | Error e -> raise (Pickle_exception e))

  let read_bytes : int -> char list m = (fun n s -> 
      assert (String.length s >= n);
      Tjr_string.split_at s n |> 
      (fun (bs,s') -> (s',Ok (bs|>BatString.explode))))

  let map f m s = (match m s with
      | (s',Ok x) -> (s',Ok (f x))
      | (s',Error e) -> (s',Error e))

end

module Examples = struct

  (* open Btree_util *)

  let p_pair : P.m -> P.m -> P.m = P.(fun p q ->
      p |> bind (fun x -> q |> bind (fun y -> ret ())))

  let u_pair : 'a U.m -> ('a -> 'b U.m) -> ('a * 'b) U.m = U.(fun p q ->
      p |> bind (fun x -> q x |> bind (fun y -> ret (x,y)))
    )

  let p_int32 : int32 -> P.m = P.(
      fun i -> Basic_marshalling.int32_to_bytes i |> write_bytes
    )

  let u_int32 : int32 U.m = U.(
      read_bytes 4 |> bind (function s -> 
          let [a;b;c;d] = s in
          ret (Basic_marshalling.bytes_to_int32 [a;b;c;d]))
    )

  let p_int i = i |>Int32.of_int|>p_int32

  let u_int = U.(u_int32 |> map Int32.to_int)

  (* assume we know the length somehow *)
  let p_string : string -> P.m = P.(
      fun s ->
        BatString.explode s|>write_bytes
    )
  let u_string: int -> string U.m = U.(
      fun n -> read_bytes n |> map BatString.implode
    )

  let p_string_w_len : string -> P.m = P.(fun s ->
      p_int (String.length s) |> bind (
        fun _ -> p_string s)
    )

  let u_string_w_len : string U.m = U.(
      u_int |> bind (fun n -> 
          read_bytes n |> map (fun bs -> BatString.implode bs )))

  let p_list : ('a -> P.m) -> 'a list -> P.m = P.(
      fun p xs ->
        (* write length *)
        p_int (List.length xs) |> bind (
          (* write list *)
          fun _ ->
            let rec loop xs = (
              match xs with
              | [] -> ret ()
              | x::xs' -> (p x |> bind (fun _ -> loop xs')))
            in
            loop xs
        ))

  let u_list : ('a U.m) -> 'a list U.m = U.(
      fun u ->
        u_int |> bind (fun n ->
            let rec loop xs = (
              (* FIXME inefficient *)
              match List.length xs < n with
                true -> u |> bind (fun x -> loop (x::xs))
              | false -> ret (List.rev xs))
            in
            loop [])
    )


end

(* FIXME not sure combining for 'a pu really buys anything *)
(*
module PU = struct
  open Examples_
  (* the combination of p and u *)
  type 'a pu = { 
    appP: 'a -> P.m;
    appU: 'a U.m
  }
  let pair: 'a pu -> 'b pu -> ('a*'b) pu = (fun p1 p2 ->
      { appP=(fun (x,y) -> p_pair (p1.appP x) (p2.appP y));
        appU=u_pair p1.appU p2.appU 
      }
  )

  let list: 'a pu -> 'a list pu = (fun pu ->
      { appP=(fun as_ -> p_list pu.appP as_);
        appU=(u_list pu.appU) }
  )

  (* use the pickler or unpickler *)
  let pickle: 'a pu -> 'a -> P.m = (fun pu x -> x|>pu.appP)
  let unpickle: 'a pu -> 'a U.m  = (fun pu -> pu.appU)

end



module PU_examples = struct

   (* in general, appP looks at argument, appU uses previous unpickle *)

  open PU
  open Examples_

  let pu_int : int pu = { appP = p_int; appU = u_int }

  let pu_pair: 'a pu -> 'b pu -> ('a * 'b) pu = (
    fun p1 p2 -> {
        appP=(fun (x,y) -> p_pair (p1.appP x) (p2.appP y)); 
        appU=u_pair p1.appU p2.appU 
      })
  (* FIXME Following can be done using 'a pu *)

  let pu_string: int -> string pu = (fun l -> 
      { appP=p_string; appU=u_string l })

  let pu_string_w_len: string pu = {
    appP=p_string_w_len;
    appU=u_string_w_len
  }

end
*)

(* example for btree.simple ---------------------------------------- *)

(* FIXME move elsewhere *)

module String_int = struct


  module KV = struct

    (* the key is actually a 16 byte hash of the full string *)
    type k = string [@@deriving yojson]
    type v = int [@@deriving yojson]

    let key_ord: k -> k -> int = Pervasives.compare
    let equal_value : v -> v -> bool = (=)

  end

  (* let _ = (module KV: Btree_api.KV) *)


  open KV
  open Examples

  (* we know the string has length 16 *)
  let key_size = 16

  let p_key : string -> P.m = (fun s -> 
      assert (String.length s = key_size);
      p_string s)


  let u_k : k U.m = (u_string key_size)

  let p_ks : k list -> P.m = p_list p_key

  let u_ks : k list U.m = u_list u_k

end

