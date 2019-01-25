(** A simple example of a kv store. *)

open Tjr_btree 
open Default_filename

let _ = 
  Printf.printf 
    "%s ----------------------------------------------\n%!" 
    __MODULE__

let _ = Test.disable()
let _ = Isa_test.disable_isa_checks()

let max = 10000

include struct
  open Examples
  open Internal
  let { from_file; close; rest } = ii_map_on_fd
end

(* TODO this would be much faster if we used insert_many *)

(* create and init store, write some values, and close *)
let do_write () = 
  Printf.printf "Executing %d writes...\n%!" max;
  print_endline "Writing...";
  let ref_ = ref (from_file ~fn ~create:true ~init:true) in
  let ops = (rest ~ref_).imperative_ops in
  (* write values *)
  for x=1 to max do
    ops.insert x x;
  done;
  close !ref_;
  ()

(* open store, delete some values, and close *)
let do_delete () = 
  print_endline "Deleting...";
  let ref_ = ref (from_file ~fn ~create:false ~init:false) in
  let ops = (rest ~ref_).imperative_ops in
  for x=100 to 200 do
    ops.delete x;
  done;
  close !ref_;
  ()

(* open store and check whether various keys and values are correct *)
let do_check () = 
  print_endline "Checking...";
  let ref_ = ref (from_file ~fn ~create:false ~init:false) in
  let ops = (rest ~ref_).imperative_ops in
  assert(ops.find 100 = None);
  assert(ops.find 1000 = Some 1000);
  close !ref_;
  ()

(* actually execute the above *)
let _ = 
  do_write();
  do_delete();
  do_check();
  ()


let do_full_check () = 
  print_endline "Full check...";
  let ref_ = ref (from_file ~fn ~create:false ~init:false) in
  let ops = (rest ~ref_).imperative_ops in
  for x = 1 to max do
    if (100 <= x && x <= 200) then
      assert(ops.find x = None)
    else
      assert(ops.find x = Some(x))
  done;
  close !ref_;
  ()

let _ = do_full_check ()
