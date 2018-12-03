val run_test : ((unit -> unit) -> unit) ref
val test : (unit -> unit) -> unit
val enable : unit -> unit
val disable : unit -> unit
val log_messages : string list ref
val log : string -> unit
val warn : string -> unit
val print_logs : unit -> unit
