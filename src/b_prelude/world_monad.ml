(* the world ---------------------------------------- *)

module World = struct

  module FS_ = Functional_store
  type t = FS_.t
  type 'a r = 'a FS_.T_.r
             
  type 'a m = ('a,t) State_error_monad.Sem.m

  let return: 'a -> 'a m = (fun x -> (fun s -> (s,Ok x)))

  let bind: ('a -> 'b m) -> 'a m -> 'b m = (
    fun f x -> (
        fun s -> match x s with
          | (s',Error e) -> (s',Error e)
          | (s',Ok y) -> (f y s')
      ))

  let mk_ref: 'a -> 'a r m = (fun x s -> 
      FS_.mk_ref x s |> (fun (s',r) -> (s',Ok r)))

  let set: 'a r -> 'a -> unit m = (fun r v s -> 
      FS_.set r v s |> (fun s' -> (s',Ok())))

  let get: 'a r -> 'a m = (fun r s ->
      FS_.get r s |> (fun v -> (s,Ok v)))

end
