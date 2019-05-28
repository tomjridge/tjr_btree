(** Single entry point for executable examples *)
open Int_int_map_example_functionality[@@warning "-33"]

let args = Sys.argv |> Array.to_list |> List.tl

(* let _ = Test.disable() *)
let _ = 
  Isa_btree.disable_isa_checks();
  (* Isa_btree.enable_isa_checks(); *)
  Tjr_lib.Test.disable ();
  ()



(* set up profilers ------------------------------------------------- *)

let btree_main_profiler = ref dummy_profiler

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
  let now = Core.Time_stamp_counter.(fun () ->
          now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x)
  in
  begin
    Tjr_profile.string_profiler := Tjr_profile.make_string_profiler ~now;
    let open Isa_export_wrapper in
    Internal_leaf_impl.leaf_profiler := Tjr_profile.make_string_profiler ~now;
    Internal_node_impl.node_profiler := Tjr_profile.make_string_profiler ~now;
    Internal_frame_impl.frame_profiler := Tjr_profile.make_string_profiler ~now;
    export_profiler := Tjr_profile.make_string_profiler ~now;
    btree_main_profiler := Tjr_profile.make_string_profiler ~now
  end



(* measure overall time --------------------------------------------- *)

(*
let _ = 
  do_mini_check(); 
  do_full_check() 
*)


let _ = 
  profile "aa" @@ fun () ->
  Int_int_map_main.main args


(* stats ------------------------------------------------------------ *)

let _ = 
  Printf.printf "Block stats: read_count:%d, write_count:%d\n%!" 
    (!Examples.On_disk_blk_dev.read_count) 
    (!Examples.On_disk_blk_dev.write_count) 

let _ = 
  let f ref_ = !ref_.print_summary(); print_endline "" in
  let open Isa_export_wrapper in
  List.iter f [
    Tjr_profile.string_profiler;
    Internal_leaf_impl.leaf_profiler;
    Internal_node_impl.node_profiler;
    Internal_frame_impl.frame_profiler;
    export_profiler;
    btree_main_profiler];
  ()

