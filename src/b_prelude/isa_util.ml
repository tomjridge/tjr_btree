let dest_Ok x = Our.Util.(
  match x with
  | Ok y -> y
  | _ -> failwith "dest_Ok")

let rresult_to_result = Our.Util.(fun x ->
    match x with
    | Ok y -> Pervasives.Ok y
    | Error (String_error x) -> Pervasives.Error x)


