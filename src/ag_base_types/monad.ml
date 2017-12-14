(** Simple state-passing monad *)

(** The monad type, a function from an initial state to a final state,
   and a possible result of type ['a] (or an error). *)
type ('a,'s) m = 's -> 's * ('a,string) result

let bind : ('a -> ('b,'s) m) -> ('a,'s)m -> ('b,'s)m = fun f x s ->
  match x s with
  | (s',Ok y) -> f y s'
  | (s',Error e) -> (s',Error e)

let return x = fun s -> (s,Ok x)

let err e = fun s -> (s,Error e)

let run s = fun m -> m s

let fmap f x = x |> bind (fun r -> return (f r))


(** Monadic reference operations *)
type ('a,'s) mref = {
  get: unit -> ('a,'s) m;
  set: 'a -> (unit,'s) m
}
