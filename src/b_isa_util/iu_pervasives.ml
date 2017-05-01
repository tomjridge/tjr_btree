module IE = Isa_export

type 'a res = 'a Isa_export.Util.res
type ('a,'t) m = 't -> ('t * 'a res)

let check_some f x = (match x with Some x -> f x | None -> true)

let option_map f = function Some x -> Some(f x) | _ -> None

let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

let is_None x = (x = None)

let is_Some x = not(is_None x)

