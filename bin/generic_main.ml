(* a map from int to int, backed by file ------------------------------- *)

open Tjr_seq

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

open Examples
open Blk_layer

let monad_ops = Imperative.monad_ops
let ( >>= ) = monad_ops.bind
let return = monad_ops.return

let run = Tjr_monad.Imperative.of_m 

let generic_main ~example ~args ~k_of_string ~v_of_string ~k_to_string ~v_to_string ~int_to_k ~int_to_v =
  let { map_ops_with_ls; empty_leaf_as_blk; blk_allocator_ref; btree_root_ref; _ } = example () in
  Random.self_init ();
  let create,init = false,false in (* defaults *)
  let { from_file; close } = make_from_file_and_close ~monad_ops:imperative_monad_ops ~blk_ops ~empty_leaf_as_blk in 
  (* link from_file and close to the example refs *)
  let from_file ~fn ~create ~init =
    from_file ~fn ~create ~init >>= fun (fd,blk_allocator_state,btree_root_state) -> 
      blk_allocator_ref:=blk_allocator_state;
      btree_root_ref:=btree_root_state;
      let close () = close ~fd ~blk_allocator_state:(!blk_allocator_ref) ~btree_root_state:(!btree_root_ref) in
      return (fd,close)
  in
  match args with
  | ["init"; fn] -> run (
      from_file ~fn ~create:true ~init:true >>= fun (_fd,close) -> 
      print_endline "init ok";
      close ())

  | ["count"; fn] -> run (
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls fd in
      let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
      make_leaf_stream (!btree_root_ref) >>= fun lss -> 
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
      Printf.printf "count ok; %d entries\n%!" !count;
      close ())

  | ["insert";fn;k;v] -> run (
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls fd in
      insert ~k:(k_of_string k) ~v:(v_of_string v) >>= fun () -> 
      close ())

  | ["delete";fn;k] -> run (
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ delete; _ } = map_ops_with_ls fd in
      delete ~k:(k_of_string k) >>= fun () -> 
      close ())

  | ["list";fn] -> run (
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls fd in
      let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
      make_leaf_stream (!btree_root_ref) >>= fun lss -> 
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
      print_endline "list ok";
      close ())

  | ["insert_range";fn;l;h] -> run (
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls fd in
      let l,h = int_of_string l, int_of_string h in
      (* NOTE this times the inserts, not the init and close *)
      let f () =           
        l |> List_.iter_break (fun l -> 
            match l>h with 
            | true -> `Break ()
            | false -> (
                let h' = min (l+chunksize) h in
                let kvs = int_range ~f:(fun k -> (int_to_k k,int_to_v @@ 2*k)) l h' in
                run (insert_all ~kvs);
                `Continue (h'+1)))
      in
      measure_execution_time_and_print "insert_range" f;
      print_endline "insert_range ok";
      close ())

  | ["test_random_reads";fn;l;h;n] -> run (
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ find; _ } = map_ops_with_ls fd in
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random reads between >=l and <h *)
      let d = h - l in
      let f () = 
        1 |> List_.iter_break (fun i -> 
          match i > n with 
            | true -> `Break()
            | false -> 
              let k = l+(Random.int d) in
              ignore(run (find ~k));
              `Continue(i+1))
      in
      f ();
      print_endline "test_random_reads ok";
      close ())

  | ["test_random_writes";fn;l;h;n] -> run (
      (* version using plain insert *)
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls fd in
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random writes between >=l and <h *)
      let d = h - l in
      let f () = 
        1 |> List_.iter_break (fun i -> 
            match i > n with 
            | true -> `Break()
            | false -> 
              let k = l+(Random.int d) in 
              let (k,v) = (int_to_k k,2*k|>int_to_v) in
              run (insert ~k ~v);
              `Continue(i+1))
      in
      measure_execution_time_and_print "test_random_writes" f;
      print_endline "test_random_writes ok";
      close ())

  | ["test_random_writes_im";fn;l;h;n] -> (
      (* version using insert_many, with sorting and chunks *)
      from_file ~fn ~create ~init >>= fun (fd,close) -> 
      let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls fd in
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random writes between >=l and <h *)
      let d = h - l in
      let f () = 
        1 |> List.iter_break (fun i -> 
            match i > n with
            | true -> `Break()
            | false -> 
              let k = l+(Random.int d) in 
              let (k,v) = (int_to_k k,2*k|>int_to_v) in              
              run (insert_all 
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
