(* general util stuff ---------------------------------------- *)

module Int = struct
  type t = int 
  let compare: t -> t -> int = Pervasives.compare 
end

module Set_int = Set.Make(Int)
module Map_int = Map.Make(Int)
module Map_string = Map.Make(String)


let dest_Ok x = Our.Util.(
  match x with
  | Ok y -> y
  | _ -> failwith "dest_Ok")

let rresult_to_result = Our.Util.(fun x ->
    match x with
    | Ok y -> Pervasives.Ok y
    | Error (String_error x) -> Pervasives.Error x)


let impossible (x:string) = failwith ("impossible: "^x)

let dest_Some x = match x with | Some x -> x | _ -> failwith "dest_Some"

let flush_out () = flush Pervasives.stdout

let read_file fn = (BatPervasives.input_file fn)

