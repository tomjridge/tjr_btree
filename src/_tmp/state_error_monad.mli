module Lens = Lens.Lens
module Mut :
  sig
    type 'a t = { set : 'a -> unit; get : unit -> 'a; }
    val from_ref : 'a ref -> 'a t
  end
module State_error_monad :
  sig
    type ('a, 's) m = 's -> 's * ('a, string) result
    val return : 'a -> ('a, 's) m
    val bind : ('a -> ('b, 's) m) -> ('a, 's) m -> ('b, 's) m
    val fmap : ('a -> 'b) -> ('a, 's) m -> ('b, 's) m
    val run : 's -> ('a, 's) m -> 's * ('a, string) result
    val unsafe_run : 's Mut.t -> ('a, 's) m -> 'a
    val run_ref : 's ref -> ('a, 's) m -> 'a
    val with_lens : ('l, 's, 'r) Lens.t -> ('a, 's) m -> ('a, 'l) m
    val run_list : 's -> (unit, 's) m list -> 's * (unit, string) result
    val err : string -> ('a, 's) m
    val safely : string -> ('a, 's) m -> ('a, 's) m
    val assert_ok : 's * ('a, string) result -> unit
    val assert_ : string -> bool -> (unit, 's) m
  end
module Sem = State_error_monad
