module Lens = Lens.Lens

(* mutable references *)
module Mut = struct
  type 'a t = {
    set: 'a -> unit;
    get: unit -> 'a
  }
  let from_ref r = { set=(fun x -> r:=x); get=(fun () -> !r) }
end

module State_error_monad : sig
  type ('a,'s) m = 's -> ('s * ('a,string) result)
  val return: 'a -> ('a,'s) m
  val bind: ('a -> ('b,'s) m) -> ('a,'s) m -> ('b,'s) m
  val fmap: ('a -> 'b) -> ('a,'s) m -> ('b,'s) m
  val run: 's -> ('a,'s) m -> 's * ('a,string) result
  val unsafe_run: 's Mut.t -> ('a,'s) m -> 'a (* mainly for testing *)
  val run_ref: 's ref -> ('a,'s) m -> 'a (* mainly for testing *)
  val with_lens: ('l,'s,'r) Lens.t -> ('a,'s) m -> ('a,'l) m
(*  val get: ('s,'s) m  *)
  val run_list: 's -> (unit,'s) m list -> 's * (unit,string) result
  val err: string -> ('a,'s) m
  val safely: string -> ('a,'s) m -> ('a,'s) m
  val assert_ok: ('s * ('a,string) result) -> unit
  val assert_: string->bool -> (unit,'s) m
end = struct
  type ('a,'s) m = 's -> ('s * ('a,string) result)


  let return: 'a -> ('a,'s) m = (fun x -> (fun s -> (s,Ok x)))

  let bind: ('a -> ('b,'s) m) -> ('a,'s) m -> ('b,'s) m = (
    fun f x -> (
        fun s -> match x s with
          | (s',Error e) -> (s',Error e)
          | (s',Ok y) -> (f y s')
      ))

  let err e = (fun s -> (s,Error e))

  let assert_ s b = (
    if b then return () else err s)

  let fmap: ('a -> 'b) -> ('a,'s) m -> ('b,'s) m = (
    fun f -> fun m -> fun s ->
      m s |> 
      (fun (s',r) -> (s',
                      match r with
                      | Ok x-> Ok (f x)
                      | Error e -> Error e)))
      
  let run: 's -> ('a,'s) m -> 's * ('a,string) result = (fun s f -> f s)
                                                        
  let unsafe_run s m = Mut.(m |> run (s.get()) |> (fun (s',res) -> 
      s.set(s'); match res with Ok x -> x | Error e -> failwith (__LOC__ ^ e)))

  let run_ref s m = unsafe_run (Mut.from_ref s) m

  (* lifting to another state monad *)
  let with_lens: ('l,'s,'r) Lens.t -> ('a,'s) m -> ('a,'t) m = (fun l m ->
      fun t -> 
        l.from t |> 
        (fun (s,r) -> 
           run s m 
           |> (fun (s',res) -> (l.to_ (s',r),res))))

(*
  let get: ('s,'s) m = (fun s -> (s,Ok s)) 
*)

  let rec run_list s xs = (
    match xs with
    | [] -> (s,Ok())
    | x::xs' -> (
        x|>run s|>(fun (s',res) ->
            match res with
            | Ok () -> (run_list s' xs')
            | Error e -> (s',Error e))
      ))

  let safely = (
    fun msg m ->
      fun s -> 
        try m s 
        with e -> (s,Error (msg ^ (Printexc.to_string e))))

  let assert_ok m = (
    match m with
    | (s,Ok x) -> ()
    | _ -> failwith (__LOC__ ^ "result was not ok"))
end

(* short name *)
module Sem = State_error_monad
