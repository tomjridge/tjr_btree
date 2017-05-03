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

(** Get a file descriptor corresponding to a file. Possibly create the
   file, and init it (truncate it to 0) *)
let fd_from_file ~fn ~create ~init = Unix.(
    let flgs = [O_RDWR] @ (if create then [O_CREAT] else []) in
    openfile fn flgs 0o640 
    |> (fun fd -> (if init then ftruncate fd 0 else ()) |> (fun _ -> fd))
  )

(** The missing function to read a file and return a string. *)
let read_file fn = (BatPervasives.input_file fn)

(*
let rec iter_step (f:'s -> 's option) (x:'s) = (
  let s' = f x in
  match s' with
  | None -> x
  | Some x' -> iter_step f x')
*)
