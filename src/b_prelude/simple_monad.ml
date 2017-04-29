(* simple state-passing monad ---------------------------------------- *)

type ('a,'s) m = 's -> 's * ('a,string) result

let bind : ('a -> ('b,'s) m) -> ('a,'s)m -> ('b,'s)m = fun f x s ->
  match x s with
  | (s',Ok y) -> f y s'
  | (s',Error e) -> (s',Error e)

let return x = fun s -> (s,Ok x)

let err e = fun s -> (s,Error e)
