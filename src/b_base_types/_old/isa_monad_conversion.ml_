(*

(* isa_btree has ('a,'t) MM = 't -> ('t * 'a res); this handles
   unexpected errors only; here we map to the step monad, assuming
   some way of handling unexpected errors *)


type error_msg = string

(* the type of the isabelle monad *)
type ('a,'t) isa_m = 't -> ('t * ('a,error_msg) result)


(* we want to convert one of these to ('a,'t) Tjr_step_monad.m *)

open Tjr_step_monad

let get_world,set_world = Step_monad_implementation.(get_world,set_world)

let x_isa_monad ~(handle_error:error_msg -> ('a,'t)m) (m:('a,'t)isa_m) = 
  get_world () |> bind @@ fun t ->
  m t |> fun (t',r) ->
  set_world t' |> bind @@ fun () ->
  match r with    
  | Ok a -> return a
  | Error msg -> handle_error msg
*)
