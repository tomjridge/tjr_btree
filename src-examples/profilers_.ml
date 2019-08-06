[%%import "optcomp_config.ml"]

[%%if PROFILING_ENABLED]
module Blk_profiler = Tjr_profile.With_array.Make_profiler(struct let cap = int_of_float 1e7 end)
[%%else]
module Blk_profiler = Tjr_profile.Dummy_int_profiler
[%%endif]
