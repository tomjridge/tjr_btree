(* general util stuff ---------------------------------------- *)


let flush_out () = flush Pervasives.stdout

let read_file fn = (BatPervasives.input_file fn)

(* misc ---------------------------------------- *)

let rec iter_step (f:'s -> 's option) (x:'s) = (
  let s' = f x in
  match s' with
  | None -> x
  | Some x' -> iter_step f x')
