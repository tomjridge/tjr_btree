[%%import "config.ml"]

[%%if PROFILING_ENABLED]
let _ = assert(Printf.printf "%s: profiling enabled\n%!" __FILE__; true)
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]

module Blk_profiler = struct
  let { mark; _ } = 
    if profiling_enabled then make_profiler ~print_header:"bt blk profiler" ()
    else dummy_profiler
end

module Lru_profiler = struct
  let { mark; _ } = 
    if profiling_enabled then make_profiler ~print_header:"bt lru profiler" ()
    else dummy_profiler
end
    
