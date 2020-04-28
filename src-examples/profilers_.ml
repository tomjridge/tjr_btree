[%%import "config.ml"]

[%%if PROFILING_ENABLED]
let _ : unit = Printf.printf "Profiling enabled (bt-ex/%s)\n%!" __FILE__
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]

module Blk_profiler = struct
  let { mark; _ } = 
    if profiling_enabled 
    then make_profiler 
        ~print_header:(Printf.sprintf "bt blk profiler (bt-ex/%s)" __FILE__) ()
    else dummy_profiler
end

module Lru_profiler = struct
  let { mark; _ } = 
    if profiling_enabled 
    then make_profiler 
        ~print_header:(Printf.sprintf "bt lru profiler (bt-ex/%s)" __FILE__) ()
    else dummy_profiler
end
    
