(* pervasive defns ---------------------------------------- *)

module Int = struct
  type t = int 
  let compare: t -> t -> int = Pervasives.compare 
end
module Set_int = Set.Make(Int)
module Map_int = Map.Make(Int)
module Map_string = Map.Make(String)

let impossible (x:string) = failwith ("impossible: "^x)

let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

let is_None x = (x = None)

let is_Some x = not(is_None x)

let option_map f = function Some x -> Some(f x) | _ -> None

let flush_out () = flush Pervasives.stdout

let fd_from_file ~fn ~create ~init = Unix.(
    let flgs = [O_RDWR] @ (if create then [O_CREAT] else []) in
    openfile fn flgs 0o640 
    |> (fun fd -> (if init then ftruncate fd 0 else ()) |> (fun _ -> fd))
  )

let read_file fn = (BatPervasives.input_file fn)
let rec iter_step (f:'s -> 's option) (x:'s) = (
  let s' = f x in
  match s' with
  | None -> x
  | Some x' -> iter_step f x')
