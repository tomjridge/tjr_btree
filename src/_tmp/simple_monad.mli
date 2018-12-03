type ('a, 's) m = 's -> 's * ('a, string) result
val bind : ('a -> ('b, 's) m) -> ('a, 's) m -> ('b, 's) m
val return : 'a -> 'b -> 'b * ('a, 'c) result
