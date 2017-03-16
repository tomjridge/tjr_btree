(* exhaustive state-space exploration ------------------------------ *)

(* we factor out the common code *)


module type S = sig
  module State : Set.OrderedType

  type t = State.t 
  type op (* operations *)

  val step: op -> t -> t list
  val check_invariants: t -> unit
  val check_step_invariants: t -> t -> unit
end


open Btree_util

module Make = functor (S:S) -> struct
  module S = S
  open S

  module States = Set.Make(State)
  module STS = States

  type test_state = { todo:STS.t; done_: STS.t }

  let reps = ref 0

  let step ops ts = (
    reps:=!reps+1;
    if (!reps) mod 1000 = 0 then (Printf.printf "."; flush_out ()) else ();
    if (!reps) mod 10000 = 0 then (
      Printf.printf "\ntodo: %d; done: %d " 
        (STS.cardinal ts.todo) (STS.cardinal ts.done_)) else ();
    let s = STS.choose ts.todo in
    let ts = { todo=STS.remove s ts.todo; done_=STS.add s ts.done_ } in
    let next_states = ops |> List.map (fun op -> S.step op s) |> List.concat in
    let ns = next_states |> STS.of_list in
    let ns = STS.diff ns ts.done_ in
    (* check invariants *)
    ns |> STS.iter check_invariants;
    ns |> STS.iter (fun s' -> check_step_invariants s s');
    match STS.is_empty ns with 
    | true -> None
    | false -> (
        let ts = { ts with todo=STS.union ns ts.todo } in
        Some ts))

  let test ops init = (
    let ts = ref (Some {todo=init; done_=STS.empty}) in
    while((!ts) <> None) do
      ts:=step ops (!ts|>Btree_util.dest_Some) 
    done;
    ()
  )
  
  
end
