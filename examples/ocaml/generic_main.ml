(* a map from int to int, backed by file ------------------------------- *)
open Tjr_seq
module Blk_id = Blk_id_as_int

(* for insert_many operations *)
let chunksize = 1000


let insert_seq ~sort ~insert_all ~todo ~seq_ops =
  let rec f ~todo = 
    seq_ops.take_and_drop chunksize todo |> fun (xs,todo) ->
    match xs with
    | [] -> ()
    | _ -> 
      let kvs = 
        if sort then List.sort (Pervasives.compare) xs else xs
      in 
      insert_all kvs;
      f ~todo
  in
  f ~todo

let _ = insert_seq

(* allow float representation *)
let int_of_string s = 
  float_of_string_opt s |> function
  | None -> int_of_string s
  | Some f -> int_of_float f


(* use carefully - only create a list of length chunksize; f is a function to map *)
let rec int_range ~f l h = 
  if l > h then [] else (f l)::(int_range ~f (l+1) h)

(* open Tjr_btree *)

module type S = sig
  type k
  type v
  type leaf_stream
  val int_to_k: int -> k
  val int_to_v: int -> v
  val k_to_string: k -> string
  val k_of_string: string -> k
  val v_to_string: v -> string
  val v_of_string: string -> v
  val btree_from_file: Blk_layer.btree_from_file
  val map_ops_etc:
    (k, v, Blk_id.blk_id, leaf_stream,fstore state_passing) Btree_intf.Map_ops_etc_type.map_ops_etc
end

module Make_generic_main(S:S) = struct

  open S

  let usage = {|

Usage: 
  int_int_map_main init <path>
  int_int_map_main count <path>
  int_int_map_main insert <path> <key> <value>
  int_int_map_main delete <path> <key>
  int_int_map_main list <path>
  int_int_map_main insert_range <path> <low> <high>
  int_int_map_main test_random_reads <path> <low> <high> <num>
  int_int_map_main test_random_writes <path> <low> <high> <num>
  int_int_map_main test_random_write_im <path> <low> <high> <num>

Arguments:  
  <path> is the path to the file which serves as the store.
  <low> and <high> together define a range of integers.
  <num> is the number of iterations (for test targets)

Description:
  The initial argument is the command, typically a map operation, or some test targets.
  NOTE insert_range inserts (n,2*n) for n in range
|}

  let main args =
    let leaf_stream_ops = map_ops_etc.leaf_stream_ops in
    let Isa_btree_intf.{ make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
    Random.self_init ();
    (* turn off wf checking *)
    (* Isa_test.disable_isa_checks(); *)
    (* Test.disable (); *)
    let create,init = false,false in (* defaults *)
    let Blk_layer.{ btree_from_file } = btree_from_file in
    match args with
    | ["init"; fn] ->
      btree_from_file ~fn ~create:true ~init:true |> fun { close; _ } ->
      print_endline "init ok";
      close ()

    | ["count"; fn] -> (
        btree_from_file ~fn ~create ~init
        |> fun { run; close; root_block; _ } -> 
        let Isa_btree_intf.{ make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
        run (make_leaf_stream root_block.btree_root) |> fun lss ->
        let count = ref 0 in
        let rec loop lss =
          let _ = 
            List.iter 
              (fun (_k,_v) -> incr count)
              (ls_kvs lss)
          in
          run (ls_step lss) |> function
          | None -> ()
          | Some lss -> loop lss
        in
        loop lss;
        close ();
        Printf.printf "count ok; %d entries\n%!" !count)

    | ["insert";fn;k;v] -> (
        btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
        run (map_ops_etc.insert ~k:(k_of_string k) ~v:(v_of_string v));
        close ())

    | ["delete";fn;k] -> (
        btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
        run (map_ops_etc.delete ~k:(k_of_string k));
        close ())

    | ["list";fn] -> (
        btree_from_file ~fn ~create ~init
        |> fun { run; close; root_block; _ } -> 
        run (make_leaf_stream root_block.btree_root) |> fun lss ->
        let rec loop lss =
          let _ = 
            List.iter 
              (fun (k,v) -> 
                 Printf.printf "%s -> %s\n" (k_to_string k) (v_to_string v))
              (ls_kvs lss)
          in
          run (ls_step lss) |> function
          | None -> ()
          | Some lss -> loop lss
        in
        loop lss;
        close ();
        print_endline "list ok")

    | ["insert_range";fn;l;h] -> (
        btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
        let l,h = int_of_string l, int_of_string h in
        (* NOTE this times the inserts, not the init and close *)
        measure_execution_time_and_print "insert_range" @@ (fun () -> 
          l |> List_.iter_break (fun l -> 
              match l>h with 
              | true -> `Break ()
              | false -> (
                  let h' = min (l+chunksize) h in
                  let kvs = int_range ~f:(fun k -> (int_to_k k,int_to_v @@ 2*k)) l h' in
                  run (map_ops_etc.insert_all ~kvs);
                  `Continue (h'+1))));
        close ();
        print_endline "insert_range ok";
      )

    | ["test_random_reads";fn;l;h;n] -> (
        btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random reads between >=l and <h *)
        let d = h - l in
        Tjr_seq.((1--n) |> fun (tad,todo) -> 
                 let tad = tad |> map (fun _ -> let k = l+(Random.int d) in int_to_k k) in
                 iter (fun k -> ignore(run(map_ops_etc.find ~k))) tad todo;
                 close ();
                 print_endline "test_random_reads ok"))

    | ["test_random_writes";fn;l;h;n] -> (
        (* version using plain insert *)
        btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        let d = h - l in
        let f () = 
          Tjr_seq.((1--n)  |> fun (tad,todo) -> 
                   let tad = tad |> map (fun _ -> let k = l+(Random.int d) in (int_to_k k,2*k|>int_to_v)) in
                   iter (fun (k,v) -> run(map_ops_etc.insert ~k ~v)) tad todo)
        in
        measure_execution_time_and_print "test_random_writes" f;
        close ();
        print_endline "test_random_writes ok")

    | ["test_random_writes_im";fn;l;h;n] -> (
        (* version using insert_many, with sorting and chunks *)
        btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        let d = h - l in
        Tjr_seq.((1--n) |> fun (tad,todo) -> 
                 let tad = tad |> map (fun _ -> let k = l+(Random.int d) in (int_to_k k,2*k|>int_to_v)) in
                 let insert_all = fun kvs -> run (map_ops_etc.insert_all ~kvs) in
                 insert_seq ~sort:true ~insert_all ~todo ~seq_ops:tad;
                 close ();
                 print_endline "test_random_writes_im ok"))

    (* same as test_random_writes
       | ["test_random_writes_mutate";fn;l;h;n] -> (
          (* version using insert_many, with sorting and chunks *)
          btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
          let l,h,n = int_of_string l, int_of_string h, int_of_string n in
          (* n random writes between >=l and <h *)
          let d = h - l in
          let todo = OSeq.(
              (1--n) 
              |> map (fun _ -> let k = l+(Random.int d) in (k,2*k)))
          in
          Seq.iter (fun (k,v) -> run(map_ops.insert ~k ~v)) todo;
          close ();
          print_endline "test_random_writes_mutate ok")
    *)

    | ["nop"] -> (
        (* print_endline "nop ok" *)
      )

    | ["--help"] -> print_endline usage

    | _ ->
      print_endline usage;
      Printf.sprintf "Unrecognized args: %s, at %s"
        (String.concat " " args)
        __LOC__
      |> failwith

end
