(** A simple example of a kv store; k=string, v=string. *)

open Tjr_fs_shared
open Tjr_btree
open Default_filename

let _ = 
  Printf.printf 
    "%s ----------------------------------------------\n%!" 
    __MODULE__


(* construct keys and values from an int *)
let k x = "k"^(string_of_int x) |> Small_string.of_string
let v x = "v"^(string_of_int x) |> Small_string.of_string

let max = 10000

include struct
  open Examples
  open Internal
  let { from_file; close; rest } = ss_map_on_fd
end

(* create and init store, write some values, and close *)
let do_write () = 
  print_endline "Writing...";
  (* create and initialize *)
  let ref_ = ref (from_file ~fn ~create:true ~init:true) in
  let ops = (rest ~ref_).imperative_ops in
  (* write values *)
  for x=1 to max do
    (* TODO this would be much faster if we used insert_many *)
    ops.insert (k x) (v x);
  done;
  close !ref_;
  ()


(* open store, delete some values, and close *)
let do_delete () = 
  print_endline "Deleting...";
  let ref_ = ref (from_file ~fn ~create:true ~init:false) in
  let ops = (rest ~ref_).imperative_ops in
  for x=100 to 200 do
    ops.delete (k x);
  done;
  close !ref_;
  ()


(* open store and check whether various keys and values are correct *)
let do_check () = 
  print_endline "Checking...";
  let ref_ = ref (from_file ~fn ~create:false ~init:false) in
  let ops = (rest ~ref_).imperative_ops in
  assert(ops.find (k 100) = None);
  assert(ops.find (k 1000) = Some(v 1000));
  close !ref_;
  ()


(* actually execute the above *)
let _ = 
  Printf.printf "Executing %d writes...\n" max;
  do_write();
  do_delete();
  do_check();
  ()


let do_full_check () = 
  print_endline "Full check...";
  let ref_ = ref (from_file ~fn ~create:false ~init:false) in
  let ops = (rest ~ref_).imperative_ops in
  for x = 1 to max do
    if 100 <= x && x <= 200 then
      assert(ops.find (k x) = None)
    else
      assert(ops.find (k x) = Some(v x))
  done;
  close !ref_;
  ()


let _ = do_full_check ()

