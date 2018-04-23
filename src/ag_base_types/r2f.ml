open Frame

(** Block ref to frame option *)
type ('k,'v,'r,'t) r2f = ('t -> 'r -> ('k,'v,'r) frame option) 

let run = Tjr_step_monad.Extra.run

let mk_r2f ~store_read : ('k,'v,'r,'t) r2f = (
  fun s r ->
    store_read r 
    |> run s 
    |> function (s',f) -> Some f | _ -> (ignore(failwith __LOC__); None))

let _ = mk_r2f
