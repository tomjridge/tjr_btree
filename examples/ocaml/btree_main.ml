(** Single entry point for executable examples *)

let args = Sys.argv |> Array.to_list |> List.tl

(* flag config ------------------------------------------------------ *)

module Flag_config = struct
  type config = {
    tests_enabled: bool;
    (* profiling_enabled: bool; profiling now controlled at compile time *)
  } [@@deriving yojson]
  let default_config = Some {
    tests_enabled=false;
    (* profiling_enabled=false *)
  }
  let filename="tests_and_profiling_config.json"
end

(* declares val config *)
include Tjr_config.Make(Flag_config)


(* set up test flags ------------------------------------------------ *)

let _ = 
  match config.tests_enabled with
  | true -> 
    Isa_btree.enable_isa_checks();
    Tjr_lib.Test.enable ();
    ()
  | false -> 
    (* should be disabled by default FIXME check *)
    Isa_btree.disable_isa_checks();
    Tjr_lib.Test.disable();
    ()

(* set up profilers ------------------------------------------------- *)

module Profiler = Make_profiler()
open Profiler

(* run main, measure overall time ----------------------------------- *)

let usage = {|
Usage:
  btree_main <switch> [additional main args]

<switch> - one of eg1, ii, eg2, ss: 

eg1 : int_int_map_example 
ii  : int_int_map_main 
eg2 : string_string_map_example 
ss  : string_string_map_main

For the main targets ii and ss, the additional args ... are passed to the
corresponding main.
|}
  

let _ = 
  profile "aa" @@ fun () ->
  match args with
  | [] -> (print_endline usage; exit 0)
  | ["eg1"] -> Int_int_map_example.do_all()
  | "ii"::args -> Int_int_map_main.main args
  | ["eg2"] -> String_string_map_example.do_all()
  | "ss"::args -> String_string_map_main.main args


(* stats ------------------------------------------------------------ *)

let _ = 
  let open Tjr_btree_examples.Blk_layer in
  Printf.printf "Block stats: read_count:%d, write_count:%d\n%!" 
    (!On_disk_blk_dev.read_count)
    (!On_disk_blk_dev.write_count)
    
let _ = 
  Isa_btree.Profilers.(
  match profiling_enabled with
  | false -> ()
  | true -> 
    let f (s,p) = 
      Printf.printf "\n%s\n" s; p.print_summary(); print_endline "" in
    List.iter f [
      "leaf_profiler",Leaf_profiler.internal_profiler;
      "node_profiler",Node_profiler.internal_profiler;
      "frame_profiler",Frame_profiler.internal_profiler;
      "main_profiler",Profiler.internal_profiler] )
