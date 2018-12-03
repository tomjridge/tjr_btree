module type S =
  sig
    module State : Set.OrderedType
    type op
    val step : op -> State.t -> State.t list
    val check_invariants : State.t -> unit
    val check_step_invariants : State.t -> State.t -> unit
  end
module Make :
  functor (S : S) ->
    sig
      module S :
        sig
          module State :
            sig type t = S.State.t val compare : t -> t -> int end
          type op = S.op
          val step : op -> State.t -> State.t list
          val check_invariants : State.t -> unit
          val check_step_invariants : State.t -> State.t -> unit
        end
      module States :
        sig
          type elt = S.State.t
          type t = Set.Make(S.State).t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val singleton : elt -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val max_elt : t -> elt
          val choose : t -> elt
          val split : elt -> t -> t * bool * t
          val find : elt -> t -> elt
          val of_list : elt list -> t
        end
      module STS = States
      type test_state = { todo : STS.t; done_ : STS.t; }
      val reps : int ref
      val step : S.op list -> test_state -> test_state option
      val test : S.op list -> STS.t -> unit
    end
