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
  include Tjr_btree_examples.Make_1.S
  val i2k: int -> k
  val i2v: int -> v
  val debug_k_and_v_are_int: bool
end

module Make(S:S) = struct
  open S
  module From_make_1 = Make_1.Make(S)
  open From_make_1

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

  let _ : unit = Random.self_init ()

  (* when reading, either the file exists, in which case we assume it
     is valid and proceed, or it doesn't, in which case we initialize
     it *)

  (* let filename = "btree.store" *)

  module B = Blk_id_as_int
  open B
  module type Y = sig
    include Rt_blk.T
    val blk_alloc : (r, lwt) blk_allocator_ops
    val root_ops : (blk_id, lwt)with_state
    (* include From_make_1.S *)
    val ops : (k, v, r, ls, t) Btree_intf.map_ops_with_ls
    val ls_create : unit -> (ls,t)m
    val ls_step : ls -> (ls option, t) m
    val ls_kvs : ls -> (k * v) list
    val close : unit -> (unit,t)m
  end

  let main args =
    let run x = x in
    let open_ ?flgs:(flgs=[]) fn = 
      Tjr_btree_examples.Rt_blk.open_ ~flgs ~empty_leaf_as_blk fn >>= fun from_open -> 
      let module From_open = (val from_open) in
      (* tie the rt_blk together with blk_alloc and root_ops *)
      let module Y = struct
        include From_open
        let map_ops_and_flush = From_make_1.make ~blk_dev_ops ~blk_alloc ~root_ops
        module Map_ops_and_flush = (val map_ops_and_flush)

        let ops = Map_ops_and_flush.map_ops_with_ls

        let ls_create () = ops.leaf_stream_ops.make_leaf_stream !(rt_blk.bt_rt)
        let ls_step = ops.leaf_stream_ops.ls_step
        let ls_kvs = ops.leaf_stream_ops.ls_kvs

        let close () = 
          Map_ops_and_flush.flush_cache() >>= fun () -> 
          From_open.wrt_rt_and_close()
      end
      in
      return (module Y : Y)
    in
    match args with  (* FIXME fn is already set by this point *)
    | ["init"; fn] -> run (
        open_ ~flgs:[O_TRUNC] fn >>= fun x -> 
        print_endline "init ok";
        let module X = (val x) in
        X.close ())
        
    | ["count"; fn] -> run (
        open_ fn >>= fun x -> 
        let module X = (val x) in        
        X.ls_create () >>= fun ls -> 
        let count = ref 0 in
        ls |> iter_k (fun ~k ls -> 
            count:=!count + (List.length (X.ls_kvs ls)); 
            X.ls_step ls >>= function
            | None -> return ()
            | Some lss -> k lss) >>= fun _ -> 
        Printf.printf "count ok; %d entries\n%!" !count;
        X.close ())

    | ["insert";fn;k;v] -> run (
        open_ fn >>= fun x -> 
        let module X = (val x) in        
        X.ops.insert ~k:(s2k k) ~v:(s2v v) >>= fun () -> 
        X.close ())

    | ["delete";fn;k] -> run (
        open_ fn >>= fun x -> 
        let module X = (val x) in        
        X.ops.delete ~k:(s2k k) >>= fun () -> 
        X.close ())

    | ["list";fn] -> run (
        open_ fn >>= fun x ->         
        let module X = (val x) in        
        X.ls_create () >>= fun ls -> 
        ls |> iter_k (fun ~k ls ->
            X.ls_kvs ls |> fun kvs ->
            List.iter (fun (k,v) -> 
                Printf.printf "%s -> %s\n" (k2s k) (v2s v)) kvs;
            X.ls_step ls >>= function
            | None -> return ()
            | Some lss -> k lss) >>= fun _ -> 
        print_endline "list ok";
        X.close ())

    | ["insert_range";fn;l;h] -> run (
        open_ fn >>= fun x ->
        let module X = (val x) in        
        let l,h = int_of_string l, int_of_string h in
        l |> iter_k (fun ~k l ->
            match l >= h with
            | true -> return ()
            | false -> 
              let h' = min (l+chunk_size) h in
              let kvs = List_.map_range ~f:(fun x -> int2kv x) l h' in
              X.ops.insert_all ~kvs >>= fun () ->
              k h') >>= fun () ->
        (* measure_execution_time_and_print "insert_range" f; *)
        print_endline "insert_range ok";
        X.close ())

    | ["test_random_reads";fn;l;h;n] -> run (
        open_ fn >>= fun x -> 
        let module X = (val x) in        
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random reads between >=l and <h *)
        0 |> iter_k (fun ~k:kont i -> 
            match i >= n with 
            | true -> return ()
            | false -> 
              let k = i2k (rand ~l ~h) in
              X.ops.find ~k >>= fun _ ->
              kont (i+1)) >>= fun () ->
        print_endline "test_random_reads ok";
        X.close ())

    | ["test_random_writes";fn;l;h;n] -> run (
        (* version using plain insert *)
        open_ fn >>= fun x -> 
        let module X = (val x) in        
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        0 |> iter_k (fun ~k:kont i -> 
            match i >= n with 
            | true -> return ()
            | false -> 
              let k = rand ~l ~h in 
              let (k,v) = int2kv k in
              X.ops.insert ~k ~v >>= fun () ->
              kont (i+1)) >>= fun () ->
        (* measure_execution_time_and_print "test_random_writes" f; *)
        print_endline "test_random_writes ok";
        X.close ())

    | ["test_random_writes_im";fn;l;h;n] -> run (
        (* version using insert_many, with sorting and chunks *)
        open_ fn >>= fun x -> 
        let module X = (val x) in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)        
        0 |> iter_k (fun ~k i -> 
            match i >= n with
            | true -> return ()
            | false -> 
              let h' = min (i+chunk_size) n in
              let f _ = rand ~l ~h in
              let kvs = map_range ~f i h' in
              let kvs = List.sort Stdlib.compare kvs in
              let kvs = List.map int2kv kvs in
              X.ops.insert_all ~kvs >>= fun () ->
              k h') >>= fun () ->
        (* measure_execution_time_and_print "test_random_writes_im" f; *)
        print_endline "test_random_writes_im ok";
        X.close ())

    | ["nop"] -> (print_endline "nop ok"; return ())

    | ["--help"] -> (print_endline usage; return ())

    | _ ->
      print_endline usage;
      Printf.sprintf "Unrecognized args: %s, at %s"
        (String.concat " " args)
        __LOC__
      |> failwith
      
end

module Pvt = struct
  module Int_int = struct
    include Make_1.S_int_int
    let i2k = fun i -> i
    let i2v = fun i -> i
    let debug_k_and_v_are_int = true
  end
end

module Int_int = Make(Pvt.Int_int)
