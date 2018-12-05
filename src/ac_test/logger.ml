(** A global logger. *)

let dest_Some x = match x with | Some x -> x | None -> failwith __LOC__

let logger : Tjr_log.log_ops option ref = 
  Tjr_fs_shared.Global.register ~name:(__MODULE__^".logger")
    (ref None)


(* logging --------------------------------------------------------- *)

(* FIXME logging should be elsewhere *)

let log' s = (dest_Some !logger).log s

let log f = (dest_Some !logger).log_lazy f

(*
let log_messages: (unit -> string) list ref = 
  Tjr_fs_shared.Global.register 
    ~name:"log_messages" 
    (ref [fun _ -> "initial log message"])

(** Log a thunk of type [unit -> string]. *)
let log s = (log_messages:=s::!log_messages)


let log_take_length = 
  Tjr_fs_shared.Global.register 
    ~name:"log_take_length" 
  (ref 10)


(** Print most recent log messages. Typically we only print
    when an exception occurs. Independent of enable/disable *)
let print_logs () = 
  print_endline "Logs (in chronological order): ";
  ignore (
    !log_messages
    |> Tjr_list.take (!log_take_length) |> List.rev 
    |> List.iter (fun f -> f()|>print_endline));
  print_string "// end Logs\n\n"
*)

let print_last_n () = (dest_Some !logger).print_last_n ()

(* warn ------------------------------------------------------------- *)

(** Warn with a string; warnings are always printed, and added to
    logs. The intention is that warning should never appear. *)
let warn s = 
  print_endline ("WARNING: "^s);
  flush_all();
  log' s


(* exit_hook -------------------------------------------------------- *)

let at_exit ~print = 
  Pervasives.at_exit (fun _ -> if print then print_last_n () else ())
