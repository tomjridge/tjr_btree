module Lens :
  sig
    type ('l, 's, 'r) t = { from : 'l -> 's * 'r; to_ : 's * 'r -> 'l; }
    val app : ('a, 'b, 'c) t -> ('b -> 'b) -> 'a -> 'a
    val comp : ('a, 'b, 'c) t -> ('b, 'd, 'e) t -> ('a, 'd, 'e * 'c) t
  end
