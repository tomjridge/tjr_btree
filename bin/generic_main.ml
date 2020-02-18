(* a map from int to int, backed by file ------------------------------- *)

(* for insert_many operations *)
let chunk_size = 1000

let map_range = List_.map_range

let usage = {|

Usage: 
  <xxx.exe> init <path>
  <xxx.exe> count <path>
  <xxx.exe> insert <path> <key> <value>
  <xxx.exe> delete <path> <key>
  <xxx.exe> list <path>
  <xxx.exe> insert_range <path> <low> <high>
  <xxx.exe> test_random_reads <path> <low> <high> <num>
  <xxx.exe> test_random_writes <path> <low> <high> <num>
  <xxx.exe> test_random_write_im <path> <low> <high> <num>

Arguments:  
  <path> is the path to the file which serves as the store.
  <low> and <high> together define a range of integers.
  <num> is the number of iterations (for test targets)

Description:
  The initial argument is the command, typically a map operation, or some test targets.
  NOTE insert_range inserts (n,2*n) for n in range
|}

(* open Examples *)

open Tjr_monad.With_lwt

module type S = sig
  include Make_example.S
  val i2k: int -> k
  val i2v: int -> v
  val debug_k_and_v_are_int: bool
end

module Make(S:S) = struct
  open S
  module E = Make_example.Make(S)
  open E

  (* allow float representation *)
  let int_of_string s = 
    float_of_string_opt s |> function
    | None -> int_of_string s
    | Some f -> int_of_float f
  let s2k s = s |> int_of_string |> i2k
  let s2v s = s |> int_of_string |> i2v

  let k2s k = 
    assert(debug_k_and_v_are_int);
    k |> Obj.magic |> string_of_int

  let v2s v = 
    assert(debug_k_and_v_are_int);
    v |> Obj.magic |> string_of_int

  let rand ~l ~h = 
    let d = h - l in
    let k = l+(Random.int d) in
    k

  let int2kv = fun k -> (i2k k,i2v (2*k)) 

  let _ = Random.self_init ()

  (* when reading, either the file exists, in which case we assume it
     is valid and proceed, or it doesn't, in which case we initialize
     it *)

  (* let filename = "btree.store" *)

  let main args =
    let run x = x in
    match args with  (* FIXME fn is already set by this point *)
    | ["init"; fn] -> run (
        open_ ~flag:Init_empty fn >>= fun t -> 
        print_endline "init ok";
        close t)

    | ["count"; fn] -> run (
        open_ ~flag:Init_from_b0 fn >>= fun t -> 
        let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls t in
        let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
        make_leaf_stream (!(t.bt_rt_ref) |> B.of_int) >>= fun lss -> 
        let count = ref 0 in
        lss |> iter_k (fun ~k lss -> 
            count:=!count + (List.length (ls_kvs lss)); 
            ls_step lss >>= function
            | None -> return ()
            | Some lss -> k lss) >>= fun _ -> 
        Printf.printf "count ok; %d entries\n%!" !count;
        close t)

    | ["insert";fn;k;v] -> run (
        open_ ~flag:Init_from_b0 fn >>= fun t -> 
        let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls t in
        insert ~k:(s2k k) ~v:(s2v v) >>= fun () -> 
        close t)

    | ["delete";fn;k] -> run (
        open_ ~flag:Init_from_b0 fn >>= fun t -> 
        let Map_ops_with_ls.{ delete; _ } = map_ops_with_ls t in
        delete ~k:(s2k k) >>= fun () -> 
        close t)

    | ["list";fn] -> run (
        open_ ~flag:Init_from_b0 fn >>= fun t ->         
        let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls t in
        let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
        make_leaf_stream (!(t.bt_rt_ref) |> B.of_int) >>= fun lss -> 
        lss |> iter_k (fun ~k lss ->
            ls_kvs lss |> fun kvs ->
            List.iter (fun (k,v) -> 
              Printf.printf "%s -> %s\n" (k2s k) (v2s v)) kvs;
            ls_step lss >>= function
            | None -> return ()
            | Some lss -> k lss) >>= fun _ -> 
        print_endline "list ok";
        close t)

    | ["insert_range";fn;l;h] -> run (
        open_ ~flag:Init_from_b0 fn >>= fun t ->
        let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls t in
        let l,h = int_of_string l, int_of_string h in
        l |> iter_k (fun ~k l ->
            match l >= h with
            | true -> return ()
            | false -> 
              let h' = min (l+chunk_size) h in
              let kvs = List_.map_range ~f:(fun x -> int2kv x) l h' in
              insert_all ~kvs >>= fun () ->
              k h') >>= fun () ->
        (* measure_execution_time_and_print "insert_range" f; *)
        print_endline "insert_range ok";
        close t)

    | ["test_random_reads";fn;l;h;n] -> run (
        open_ ~flag:Init_from_b0 fn >>= fun t -> 
        let Map_ops_with_ls.{ find; _ } = map_ops_with_ls t in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random reads between >=l and <h *)
        0 |> iter_k (fun ~k:kont i -> 
            match i >= n with 
            | true -> return ()
            | false -> 
              let k = i2k (rand ~l ~h) in
              find ~k >>= fun _ ->
              kont (i+1)) >>= fun () ->
        print_endline "test_random_reads ok";
        close t)

    | ["test_random_writes";fn;l;h;n] -> run (
        (* version using plain insert *)
        open_ ~flag:Init_from_b0 fn >>= fun t -> 
        let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls t in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        0 |> iter_k (fun ~k:kont i -> 
            match i >= n with 
            | true -> return ()
            | false -> 
              let k = rand ~l ~h in 
              let (k,v) = int2kv k in
              insert ~k ~v >>= fun () ->
              kont (i+1)) >>= fun () ->
        (* measure_execution_time_and_print "test_random_writes" f; *)
        print_endline "test_random_writes ok";
        close t)

    | ["test_random_writes_im";fn;l;h;n] -> run (
        (* version using insert_many, with sorting and chunks *)
        open_ ~flag:Init_from_b0 fn >>= fun t -> 
        let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls t in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)        
        0 |> iter_k (fun ~k i -> 
            match i >= n with
            | true -> return ()
            | false -> 
              let h' = min (i+chunk_size) n in
              let f _ = rand ~l ~h in
              let kvs = map_range ~f i h' in
              let kvs = List.sort Pervasives.compare kvs in
              let kvs = List.map int2kv kvs in
              insert_all ~kvs >>= fun () ->
              k h') >>= fun () ->
        (* measure_execution_time_and_print "test_random_writes_im" f; *)
        print_endline "test_random_writes_im ok";
        close t)

    | ["nop"] -> (print_endline "nop ok"; return ())

    | ["--help"] -> (print_endline usage; return ())

    | _ ->
      print_endline usage;
      Printf.sprintf "Unrecognized args: %s, at %s"
        (String.concat " " args)
        __LOC__
      |> failwith
      
end

module Int_int = struct
  include Examples.Int_int
  let i2k = fun i -> i
  let i2v = fun i -> i
  let debug_k_and_v_are_int = true
end

module Make_int_int = Make(Int_int)
