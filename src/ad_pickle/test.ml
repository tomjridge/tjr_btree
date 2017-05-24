(** Support for logging and testing. Testing can be
   enabled/disabled. Also includes [log] and [warn] logging
   support. *)


(* testing *)

let run_test : ((unit -> unit) -> unit) ref = ref (fun f -> f ())

let test f = (!run_test) f

let enable () = run_test := fun f -> f()
let disable () = run_test := fun f -> ()



(* logging *)

let log_messages: string list ref = ref ["initial log message"]

let log s = (log_messages:=s::!log_messages)

let warn s = (
  print_endline ("WARNING: "^s);
  flush_all();
  log s
)

(** Print most recent (upto 20) log messages. Typically we only print
   when an exception occurs. *)
let print_logs () = 
  ignore(print_endline "Logs: ");
  ignore (
    !log_messages
    |>Extlib.ExtList.List.take 20 |> List.rev 
    |> List.iter print_endline);
  print_string "// end Logs\n\n";
