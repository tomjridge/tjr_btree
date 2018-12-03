open Frame
(* open Tjr_monad *)

(** Block ref to frame option *)
type ('k,'v,'r,'t) r2f = ('t -> 'r -> ('k,'v,'r) frame option) 

(* let run = Tjr_step_monad.Extra.run *)

(* NOTE in the following we don't know what store_read returns
   (something in a monad) but we do know that run should convert this *)
let mk_r2f ~run ~store_read : ('k,'v,'r,'t) r2f = (
  fun s r ->
    store_read r 
    |> run s 
    |> function (_s',f) -> Some f)

let _ = mk_r2f
