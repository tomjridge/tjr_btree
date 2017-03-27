let dest_Ok x = Our.Util.(
  match x with
  | Ok y -> y
  | _ -> failwith "dest_Ok")

let rresult_to_result = Our.Util.(fun x ->
    match x with
    | Ok y -> Pervasives.Ok y
    | Error (String_error x) -> Pervasives.Error x)


module X = struct

  let int_to_nat x = Gen_isa.(x |>Big_int.big_int_of_int|>Arith.nat_of_integer)
  let int_to_int x = Gen_isa.(
      x |>Big_int.big_int_of_int|>(fun x -> Arith.Int_of_integer x))

end
