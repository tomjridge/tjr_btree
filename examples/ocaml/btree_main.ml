(** Single entry point for executable examples *)
(* open Int_int_map_example_functionality *)

let args = Sys.argv |> Array.to_list |> List.tl

(* let _ = Test.disable() *)
let _ = 
  Isa_btree.disable_isa_checks();
  Tjr_test.disable ()

let _ = 
(*
  Isa_export_wrapper.Profiler2.profiler := 
    Tjr_profile.make_string_profiler 
      ~now:Core.Time_stamp_counter.(fun () ->
          now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x);
*)

(*  Isa_export.profiler := 
    Tjr_profile.make_string_profiler 
      ~now:Core.Time_stamp_counter.(fun () ->
          now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x); *)
  
  Examples.Profiler.profiler := 
    Tjr_profile.make_string_profiler 
      ~now:Core.Time_stamp_counter.(fun () ->
          now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x);

  ()



(*
let _ = 
  do_mini_check(); 
  do_full_check() 
*)

let _ = 
  Isa_export_wrapper.Profiler2.profile "aa" @@ fun () ->
  Int_int_map_main.main args

let _ = 
      Printf.printf "Block stats: read_count:%d, write_count:%d\n%!" 
        (!Examples.On_disk_blk_dev.read_count) 
        (!Examples.On_disk_blk_dev.write_count) 

let _ = !(Examples.Profiler.profiler).print_summary()
