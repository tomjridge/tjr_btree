(* test int int map on file --------------------------------------- *)

(* this is for performance testing *)

open Prelude
open Btree_api
open Int_int_map_on_fd
open Block.Blk4096
open Default_filename
open Monad

(* test uncached ---------------------------------------- *)

let test_uncached range = (
  Examples_common.mk_example ~ps ~kk:(
    fun ~disk_ops ~store_ops ~map_ops ~imperative_map_ops ~ls_ops 
      ~from_file ~close -> 
      Printf.printf "%s: test_uncached, int map on rec. fstore, %d elts: " 
        __MODULE__ 
        (List.length range);
      flush_out();
      let s = from_file ~fn ~create:true ~init:true in
      let s = ref s in
      let xs = ref range in
      ignore (
        while (!xs <> []) do
          print_string "."; flush_out();
          let x = List.hd !xs in
          ignore (map_ops.insert x (2*x) 
                  |> run !s 
                  |> function (s',Ok()) -> s:=s');
          xs:=List.tl !xs;
        done);
      print_newline ();
      (* FIXME check result? *)
      close !s;
      ()))


(* test cached ------------------------------------------------------------ *)

(* TODO test cached *)

(*
module Test_cached = struct

  module Cached_ = Int_int_filestore.Cached

  let test_cached range = 
    Printf.printf 
      "%s: test_cached, int map on rec.fstore w api cache, %d elts: " 
      __MODULE__ 
      (List.length range);
    flush_out();
    let (s,r) = FS.from_file default_filename true true in
    let s0 = ref (r,s,Map_int.empty) in
    let run = Sem.run_ref s0 in
    let xs = ref range in
    while (!xs <> []) do
      print_string "."; flush_out ();
      let x = List.hd !xs in
      run (Cached_.Insert.insert x (2*x));
      xs:=List.tl !xs
    done;
    print_newline ();
    (* FIXME should we check res in following? *)
    run (Cached_.sync ());
    Unix.close ((!s0) |> (fun (x,y,z) -> y.fs.fd));
    ()

end

include Test_cached
*)


(* 

2016-12-06 no cache:

$ ocaml $ time ./test_ii.native
test_ii

real	1m35.895s
user	1m35.068s
sys	0m0.900s

LRU read cache is much the same

--

2016-12-08 with reasonable splitting:

timings with reasonable splitting:
$ ocaml $ time ./test_ii.native
test_ii

real	0m22.850s
user	0m22.796s
sys	0m0.072s

size of store: 12288 bytes = 3 blocks as expected

--

2016-12-09 with high-level cache and insert_many

$ ocaml $ time ./test_ii.native
test_ii

real	0m0.096s
user	0m0.092s
sys	0m0.004s



*)
