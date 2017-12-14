(** Exhaustive state-space exploration for testing *)

open Base_types



(* generic testing -------------------------------------------------- *)


type ('op,'t) test_ops = {
  step: 't -> 'op -> 't list;
  check_state: 't -> unit;
  check_step: 't -> 't -> unit
}

module Tjr_set = struct
  type ('e,'t) set_ops = {
    empty: unit -> 't;
    is_empty: 't -> bool;
    mem: 'e -> 't -> bool;
    add: 'e -> 't -> 't;
    remove: 'e -> 't -> 't;
    of_list: 'e list -> 't;
    union: 't -> 't -> 't;
    diff: 't -> 't -> 't;
    cardinal: 't -> int;
    choose: 't -> 'e;
    iter: ('e -> unit) -> 't -> unit
  }

  (* reuse OCaml's sets *)
  module Make_set_ops = functor (Ord : Set.OrderedType) -> struct
    module Set_ = Set.Make(Ord)

    let set_ops = Set_.{
        empty=(fun () -> empty); is_empty; mem; add; remove; of_list; union; diff; cardinal;choose;iter
      } 
  end
end
include Tjr_set


type 'set test_state = { todo:'set; done_: 'set }


let test ~set_ops ~test_ops = (
  let reps = ref 0 in

  let card = set_ops.cardinal in

  let step ops ts = 
    reps:=!reps+1;
    begin  (* visual feedback something that is happening *)
      match () with
      | _ when (!reps) mod 10000 = 0 ->
        Printf.printf "\ntodo: %d; done: %d " (card ts.todo) (card ts.done_)
      | _ when (!reps) mod 1000 = 0  -> Printf.printf "."; flush_out ()
      | _ -> ()
    end;
    let s = set_ops.choose ts.todo in
    let ts = { todo=set_ops.remove s ts.todo; done_=set_ops.add s ts.done_ } in
    let ns = ops |> List.map (fun op -> test_ops.step s op) |> List.concat in
    let ns = ns |> set_ops.of_list in
    let ns = set_ops.diff ns ts.done_ in
    ns |> set_ops.iter test_ops.check_state;
    ns |> set_ops.iter (fun s' -> test_ops.check_step s s');
    match set_ops.is_empty ns with 
    | true -> None
    | false -> Some { ts with todo=set_ops.union ns ts.todo }
  in

  let test ops inits = 
    let ts = ref (Some {todo=set_ops.of_list inits; done_=set_ops.empty()}) in
    while !ts <> None do
      ts:=step ops (!ts|>dest_Some) 
    done
  in

  test)
