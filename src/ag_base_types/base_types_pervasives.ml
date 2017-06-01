(** Common types and definitions. *)

(** The result type. ['a res] is equal to [Ok of 'a | Error of string] *)
type 'a res = 'a Isa_export.Util.res

(** The monad type, a function from an initial state to a final state,
   and a possible result of type ['a] (or an error). *)
type ('a,'t) m = 't -> ('t * 'a res)

(* let check_some f x = (match x with Some x -> f x | None -> true) *)

let option_map f = function Some x -> Some(f x) | _ -> None

let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

let is_None x = (x = None)

let is_Some x = not(is_None x)

