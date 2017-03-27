(* general util stuff ---------------------------------------- *)

module Int = struct
  type t = int 
  let compare: t -> t -> int = Pervasives.compare 
end

module Set_int = Set.Make(Int)
module Map_int = Map.Make(Int)
module Map_string = Map.Make(String)

let impossible (x:string) = failwith ("impossible: "^x)

let dest_Some x = match x with | Some x -> x | _ -> failwith "dest_Some"

let flush_out () = flush Pervasives.stdout

let read_file fn = (BatPervasives.input_file fn)

