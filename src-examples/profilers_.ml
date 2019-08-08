open Optcomp_config
module Blk_profiler = struct
  let { mark; _ } = 
    if profiling_enabled then make_profiler ()
    else dummy_profiler
end

module Lru_profiler = struct
  let { mark; _ } = 
    if profiling_enabled then make_profiler ()
    else dummy_profiler
end
    
