(** Some profilers. *)

(*

$(CONVENTION("""NOTE that we were trying to separate variant-specific
   (in the dune sense) details to the non-core library. For example,
   Tjr_profile relies on high-resolution kernel timers, which won't be
   present in JavaScript. However, this appears difficult to achieve,
   especially since a lot of our code depends on bin_prot for
   marshalling. Instead, we assume that some dune features like
   "variants" or "virtual libraries" can make the libraries usable
   from JavaScript.  """))

$(FIXME("""Having said that, it is not so clear that there is an easy
   replacement for bin_prot in JavaScript. Perhaps something like msgpack?

Some options:

- protocol buffers ?
- capnp
- extprot
- faraday?
- data-encoding https://gitlab.com/nomadic-labs/data-encoding

For all of these, it would be useful to have some performance figures
to compare with bin_prot.
"""))

$(COMPAT("""We use bin_prot for marshalling. Looking at the bin_prot
repo, there is a xen subdirectory, which is possibly evidence that
bin_prot is supported on Mirage. Of course, it would also be useful to support
JavaScript as well."""))

*)


[%%import "optcomp_config.ml"]
open Tjr_profile

[%%if PROFILING_ENABLED]
let _ : unit = Printf.printf "Profiling enabled (bt/%s)\n%!" __FILE__
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]

module Blk_profiler = struct
  let { mark; _ } = 
    if profiling_enabled 
    then make_profiler 
        ~print_header:(Printf.sprintf "bt blk profiler (bt/%s)" __FILE__) ()
    else dummy_profiler
end

module Read_cache_profiler = struct
  let { mark; _ } = 
    if profiling_enabled 
    then make_profiler 
        ~print_header:(Printf.sprintf "bt rc profiler (bt/%s)" __FILE__) ()
    else dummy_profiler
end


module Write_back_cache_profiler = struct
  let { mark; _ } = 
    if profiling_enabled 
    then make_profiler 
        ~print_header:(Printf.sprintf "bt wbc profiler (bt/%s)" __FILE__) ()
    else dummy_profiler
end


(*
module Lru_profiler = struct
  let { mark; _ } = 
    if profiling_enabled 
    then make_profiler 
        ~print_header:(Printf.sprintf "bt lru profiler (bt/%s)" __FILE__) ()
    else dummy_profiler
end
*)    
