(** Get a file descriptor corresponding to a file. Possibly create the
   file, and init it (truncate it to 0) *)
let fd_from_file ~fn ~create ~init = Unix.(
    let flgs = [O_RDWR] @ (if create then [O_CREAT] else []) in
    openfile fn flgs 0o640 
    |> (fun fd -> (if init then ftruncate fd 0 else ()) |> (fun _ -> fd))
  )
(* FIXME this isn't doign much *)

(** The missing function to read a file and return a string. *)
let read_file fn = (BatPervasives.input_file fn)
