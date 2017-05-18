(** Pervasive defns *)

module Int = struct
  type t = int 
  let compare: t -> t -> int = Pervasives.compare 
end
module Set_int = Set.Make(Int)
module Map_int = Map.Make(Int)
module Map_string = Map.Make(String)

(** A synonym for [failwith], used to mark supposedly impossible
   situations. Should never be called. *)
let impossible (x:string) = failwith ("impossible: "^x)

let flush_out () = flush Pervasives.stdout

module File = Bt_file

include File

(*
let rec iter_step (f:'s -> 's option) (x:'s) = (
  let s' = f x in
  match s' with
  | None -> x
  | Some x' -> iter_step f x')
*)
