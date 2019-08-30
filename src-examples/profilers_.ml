open Optcomp_config
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
    
