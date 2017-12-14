(** Exhaustive state-space exploration for testing *)

(* test config ------------------------------------------------------ *)

let fields = [
  "range_min";
  "range_max";
  "range_step";
  "min_leaf_size";
  "max_leaf_size";
  "min_node_keys";
  "max_node_keys"
]


type config = (string * int) list [@@deriving yojson]


let get (c:config) (f:string) =
  List.assoc f c  (* throw exception if not found *)


let wf_config c =
  fields|>List.iter (fun f -> ignore(get c f));
  true



  

(* note --------------------------------------------------------- *)

(*

The aim is to test the operations at the ADT level. For this, we need
to instantiate ('k,'v,'r,'t) store_ops with appropriate 'r and 't. 

Previous testing (mem_store) used 

type ('k,'v) mem = {free:int; map:('k,'v)frame Map_int.t}  

We instead choose 'r = ('k,'v) tree and 't = ().


---

Starting with the empty tree, we apply every insert/delete action
possible. We check invariants before and after the action. We also
check intermediate states. The aim is to identify:

- a particular wellformed state
- an action
- the sequence of small steps involved in executing the action (we can log all small steps)
- a resulting malformed state
- the clause of a wellformedness property which is broken (implemented via exception line number)

We then pretty-print these states.

*)


open Base_types

module type S = sig
  module State : Set.OrderedType
  open State
  type op (* operations *)

  val step: op -> t -> t list
  val check_invariants: t -> unit
  val check_step_invariants: t -> t -> unit
end


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
      ts:=step ops (!ts|>dest_Some) 
    done;
    ()
  )
  
  
end
