(** Single entry point for executable examples *)

open Int_int_map_example[@@warning "-33"]

let args = Sys.argv |> Array.to_list |> List.tl

(* flag config ------------------------------------------------------ *)

module Flag_config = struct
  type config = {
    tests_enabled: bool;
    profiling_enabled: bool;
  } [@@deriving yojson]
  let default_config = Some {
    tests_enabled=false;
    profiling_enabled=false
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

let btree_main_profiler = ref dummy_profiler
                          |> Global.register ~name:"btree_main_profiler"

module Internal_ = struct
  let profile x z =
    let profiler = btree_main_profiler in
    !profiler.mark x;
    let r = z() in
    !profiler.mark (x^"'");
    r
end
open Internal_

let _ = 
  match config.profiling_enabled with
  | false -> ()
  | true -> 
    let module M = struct
      let now = Core.Time_stamp_counter.(fun () ->
          now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x)

      let _ =
        Tjr_profile.string_profiler := Tjr_profile.make_string_profiler ~now;
        (* let open Isa_export_wrapper in *)
        let open Leaf_node_frame_impls in
        Internal_leaf_impl.leaf_profiler := Tjr_profile.make_string_profiler ~now;
        Internal_node_impl.node_profiler := Tjr_profile.make_string_profiler ~now;
        Internal_frame_impl.frame_profiler := Tjr_profile.make_string_profiler ~now;
        (* export_profiler := Tjr_profile.make_string_profiler ~now; *)
        btree_main_profiler := Tjr_profile.make_string_profiler ~now;
        ()
    end
    in
    ()

(* let _ = Init_ref.set_post_init () *)

(* run main, measure overall time ----------------------------------- *)

let usage = {|
Usage:
  btree_main <n> ...

<n> - one of: 

eg1 int_int_map_example 
ii int_int_map_main 
eg2 string_string_map_example 
ss string_string_map_main

For the main targets, the additional args ... are passed to the
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
  match config.profiling_enabled with
  | false -> ()
  | true -> 
    !Tjr_profile.string_profiler.print_summary(); print_endline "";
    (* let open Init_ref in *)
    let f ref_ = !ref_.print_summary(); print_endline "" in
    (* let open Isa_export_wrapper in *)
    let open Leaf_node_frame_impls in
    List.iter f [
      Internal_leaf_impl.leaf_profiler;
      Internal_node_impl.node_profiler;
      Internal_frame_impl.frame_profiler;
      (* export_profiler; *)
      btree_main_profiler];
    ()

