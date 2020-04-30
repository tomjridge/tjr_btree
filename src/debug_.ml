[%%import "optcomp_config.ml"]

[%%if DEBUG_ENABLED]
let _ : unit = assert(Printf.printf "%s: debug enabled\n%!" __FILE__; true)
let debug_enabled = true
[%%else]
let debug_enabled = false
[%%endif]
