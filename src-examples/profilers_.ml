[%%import "config.ml"]

[%%if PROFILING_ENABLED]
let _ : unit = assert(Printf.printf "%s: profiling enabled\n%!" __FILE__; true)
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]

module Blk_profiler = struct
  let { mark; _ } = 
    if profiling_enabled then make_profiler ~print_header:(Printf.sprintf "bt blk profiler %s" __LOC__) ()
    else dummy_profiler
end

module Lru_profiler = struct
  let { mark; _ } = 
    if profiling_enabled then make_profiler ~print_header:(Printf.sprintf "bt lru profiler %s" __LOC__) ()
    else dummy_profiler
end
    
