type op = Insert of int | Delete of int  [@@deriving yojson]


let op2s op = op |> op_to_yojson |> Yojson.Safe.pretty_to_string 

