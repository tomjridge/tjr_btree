(** A simple example of a kv store. *)

open Small_string.O
(* SS is now an alias for Small_string *)

open Ss_ss_map_on_fd

(* filename *)
let fn = Default.default_filename

(* construct keys and values from an int *)
let k x = "k"^(string_of_int x) |> SS.of_string
let v x = "v"^(string_of_int x) |> SS.of_string

(* create and init store, write some values, and close *)
let do_write () = (
  print_endline "Writing...";
  (* create and initialize *)
  let s = ref (from_file ~fn ~create:true ~init:true) in
  (* get map operations *)
  let map_ops = imperative_map_ops s in
  (* write values *)
  for x=1 to 1000 do
    (* TODO this would be much faster if we used insert_many *)
    map_ops.insert (k x) (v x);
  done;
  (* close *)
  close !s
)

(* open store, delete some values, and close *)
let do_delete () = (
  print_endline "Deleting...";
  let s = ref (from_file ~fn ~create:false ~init:false) in
  let map_ops = imperative_map_ops s in
  for x=100 to 200 do
    map_ops.delete (k x);
  done;
  close !s
)

(* open store and check whether various keys and values are correct *)
let do_check () = (
  print_endline "Checking...";
  let s = ref (from_file ~fn ~create:false ~init:false) in
  let map_ops = imperative_map_ops s in
  assert(map_ops.find (SS.of_string("k100")) = None);
  assert(map_ops.find (SS.of_string("k1000")) = Some(SS.of_string("v1000")));
  close !s
)

(* actually execute the above *)
let _ = (
  do_write();
  do_delete();
  do_check()
)
