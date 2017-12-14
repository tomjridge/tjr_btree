(** Support for logging and testing. Testing can be
   enabled/disabled. Also includes [log] and [warn] logging
   support. *)


(* testing ---------------------------------------------------------- *)

let run_test : ((unit -> unit) -> unit) ref = ref (fun f -> f ())

let test f = (!run_test) f
let enable () = run_test := fun f -> f()
let disable () = run_test := fun f -> ()



(* logging --------------------------------------------------------- *)

let log_messages: (unit -> string) list ref = ref [fun _ -> "initial log message"]

(** Log a thunk of type [unit -> string]. *)
let log s = (log_messages:=s::!log_messages)

(** Print most recent (upto 20) log messages. Typically we only print
    when an exception occurs. Independent of enable/disable *)
let print_logs () = 
  print_endline "Logs (in chronological order): ";
  ignore (
    !log_messages
    |> Extlib.ExtList.List.take 20 |> List.rev 
    |> List.iter (fun f -> f()|>print_endline));
  print_string "// end Logs\n\n"


(* warn ------------------------------------------------------------- *)

(** Warn with a string; warnings are always printed, and added to
    logs. The intention is that warning should never appear. *)
let warn s = 
  print_endline ("WARNING: "^s);
  flush_all();
  log (fun _ -> s)




(* exit_hook -------------------------------------------------------- *)

let exit_hooks = ref []

let add_exit_hook (f:unit -> unit) = exit_hooks := f::!exit_hooks

let run_exit_hooks () = 
  List.iter (fun f -> f()) !exit_hooks
