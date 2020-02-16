(*
(* a map from int to int, backed by file ------------------------------- *)

(* for insert_many operations *)
let chunk_size = 1000

let map_range = List_.map_range

let _ = 
  let f x = x in
  assert(map_range ~f 1 1 = [1]);
  assert(map_range ~f 1 0 = []);
  assert(map_range ~f 1 2 = [1;2]);
  ()

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

open Examples

open Tjr_monad.With_lwt

module type S = sig
  include Examples.S
  val i2k: int -> k
  val i2v: int -> v
end

module Make(X48:S) = struct
  open X48
  module X50 = Examples.Make(X48)
  open X50

  (* allow float representation *)
  let int_of_string s = 
    float_of_string_opt s |> function
    | None -> int_of_string s
    | Some f -> int_of_float f

  let rand ~l ~h = 
    let d = h - l in
    let k = l+(Random.int d) in
    k

  let int2kv = fun k -> (i2k k,i2v (2*k)) 

  let _ = Random.self_init ()

  (* when reading, either the file exists, in which case we assume it
     is valid and proceed, or it doesn't, in which case we initialize
     it *)

  let filename = "btree.store"

  let main args = X50.mk_blk_dev_ops ~filename >>= fun x -> 
    let module X = struct 
      module X76 = (val x)
      open X76
          
      module X79 = X50.Make_2(struct let blk_dev_ops = blk_dev end)
      open X79

      let init = false  (* FIXME *)

      module Y = X50
      module Z = X79

      let _ = 
        begin
          match init with
          | false -> return ()
          | true -> 
            (from_lwt @@ Lwt_unix.ftruncate fd 0) >>= fun () ->
            (* set initial roots; FIXME this should be in X2 already *)
            Y.blk_alloc_ref:=2;
            Y.bt_rt_ref:=1;
            Z.initialize_blk_dev ()
        end >>= fun () -> 
        (* read root block *)
        Z.init_refs_from_root_block () >>= fun () ->    
        let close () = 
          Z.flush_cache() >>= fun () ->
          Z.write_refs_to_root_block () >>= fun () ->
          (from_lwt @@ Lwt_unix.close fd)
        in
        let init = false in  (* default *)
        let run x = x in
        match args with  (* FIXME fn is already set by this point *)
        | ["init"; fn] -> run (
            print_endline "init ok";
            close ())

    | ["count"; fn] -> run (
        from_file ~fn ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls ~note_cached:() fd in
        let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
        make_leaf_stream ((!btree_root_ref).btree_root) >>= fun lss -> 
        let count = ref 0 in
        let rec loop lss =
          count:=!count + (List.length (ls_kvs lss));        
          run (ls_step lss) |> function
          | None -> ()
          | Some lss -> loop lss
        in
        loop lss;
        Printf.printf "count ok; %d entries\n%!" !count;
        close ())

    | ["insert";fn;k;v] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls ~note_cached:() fd in
        insert ~k:(s2k k) ~v:(s2v v) >>= fun () -> 
        close ())

    | ["delete";fn;k] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ delete; _ } = map_ops_with_ls ~note_cached:() fd in
        delete ~k:(s2k k) >>= fun () -> 
        close ())

    | ["list";fn] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls ~note_cached:() fd in
        let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
        make_leaf_stream ((!btree_root_ref).btree_root) >>= fun lss -> 
        let rec loop lss =
          ls_kvs lss |> (fun kvs -> 
              List.iter
                (fun (k,v) -> 
                   Printf.printf "%s -> %s\n" (k2s k) (v2s v))
                kvs;
              run (ls_step lss) |> function
              | None -> ()
              | Some lss -> loop lss)
        in
        loop lss;
        print_endline "list ok";
        close ())

    | ["insert_range";fn;l;h] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h = int_of_string l, int_of_string h in
        let f () =           
          l |> iter_break (fun l -> 
              match l>=h with 
              | true -> Break ()
              | false -> 
                let h' = min (l+chunk_size) h in
                let kvs = List_.map_range ~f:(fun x -> int2kv x) l h' in
                run (insert_all ~kvs);
                Cont h')
        in
        measure_execution_time_and_print "insert_range" f;
        print_endline "insert_range ok";
        close ())

    | ["test_random_reads";fn;l;h;n] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ find; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random reads between >=l and <h *)
        let f () = 
          0 |> iter_break (fun i -> 
              match i >= n with 
              | true -> Break()
              | false -> 
                let k = i2k (rand ~l ~h) in
                ignore(run (find ~k));
                Cont(i+1))
        in
        f ();
        print_endline "test_random_reads ok";
        close ())

    | ["test_random_writes";fn;l;h;n] -> run (
        (* version using plain insert *)
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        let f () = 
          0 |> iter_break (fun i -> 
              match i >= n with 
              | true -> Break()
              | false -> 
                let k = rand ~l ~h in 
                let (k,v) = int2kv k in
                run (insert ~k ~v);
                Cont(i+1))
        in
        measure_execution_time_and_print "test_random_writes" f;
        print_endline "test_random_writes ok";
        close ())

    | ["test_random_writes_im";fn;l;h;n] -> run (
        (* version using insert_many, with sorting and chunks *)
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        let f () = 
          0 |> iter_break (fun i -> 
              match i >= n with
              | true -> Break()
              | false -> 
                let h' = min (i+chunk_size) n in
                let f _ = rand ~l ~h in
                let kvs = map_range ~f i h' in
                let kvs = List.sort Pervasives.compare kvs in
                let kvs = List.map int2kv kvs in
                run (insert_all ~kvs);
                Cont h')
        in
        measure_execution_time_and_print "test_random_writes_im" f;
        print_endline "test_random_writes_im ok";
        close ())

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
    in 
    ()

;;

  module From_file(R6: Blk_dev_factory.R6) = struct
    open R6

  end


(*
    (* init *)
    begin
      match init with
      | false -> return ()
      | true -> 
        (from_lwt @@ Lwt_unix.ftruncate fd 0) >>= fun () ->
        (* set initial roots; FIXME this should be in X2 already *)
        Y.blk_alloc_ref:=2;
        Y.bt_rt_ref:=1;
        X2.initialize_blk_dev ()
    end >>= fun () -> 
    (* read root block *)
    X2.init_refs_from_root_block () >>= fun () ->    
    let close () = 
      X2.flush_cache() >>= fun () ->
      X2.write_refs_to_root_block () >>= fun () ->
      (from_lwt @@ Lwt_unix.close fd)
    in
    return close

  let run m = Lwt_main.run (to_lwt m)

  let init = false  (* default *)

  let main args = match args with
    | ["init"; fn] -> run (
        from_file ~fn ~init:true >>= fun close -> 
        print_endline "init ok";
        close ())

    | ["count"; fn] -> run (
        from_file ~fn ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls ~note_cached:() fd in
        let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
        make_leaf_stream ((!btree_root_ref).btree_root) >>= fun lss -> 
        let count = ref 0 in
        let rec loop lss =
          count:=!count + (List.length (ls_kvs lss));        
          run (ls_step lss) |> function
          | None -> ()
          | Some lss -> loop lss
        in
        loop lss;
        Printf.printf "count ok; %d entries\n%!" !count;
        close ())

    | ["insert";fn;k;v] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls ~note_cached:() fd in
        insert ~k:(s2k k) ~v:(s2v v) >>= fun () -> 
        close ())

    | ["delete";fn;k] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ delete; _ } = map_ops_with_ls ~note_cached:() fd in
        delete ~k:(s2k k) >>= fun () -> 
        close ())

    | ["list";fn] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls ~note_cached:() fd in
        let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
        make_leaf_stream ((!btree_root_ref).btree_root) >>= fun lss -> 
        let rec loop lss =
          ls_kvs lss |> (fun kvs -> 
              List.iter
                (fun (k,v) -> 
                   Printf.printf "%s -> %s\n" (k2s k) (v2s v))
                kvs;
              run (ls_step lss) |> function
              | None -> ()
              | Some lss -> loop lss)
        in
        loop lss;
        print_endline "list ok";
        close ())

    | ["insert_range";fn;l;h] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h = int_of_string l, int_of_string h in
        let f () =           
          l |> iter_break (fun l -> 
              match l>=h with 
              | true -> Break ()
              | false -> 
                let h' = min (l+chunk_size) h in
                let kvs = List_.map_range ~f:(fun x -> int2kv x) l h' in
                run (insert_all ~kvs);
                Cont h')
        in
        measure_execution_time_and_print "insert_range" f;
        print_endline "insert_range ok";
        close ())

    | ["test_random_reads";fn;l;h;n] -> run (
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ find; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random reads between >=l and <h *)
        let f () = 
          0 |> iter_break (fun i -> 
              match i >= n with 
              | true -> Break()
              | false -> 
                let k = i2k (rand ~l ~h) in
                ignore(run (find ~k));
                Cont(i+1))
        in
        f ();
        print_endline "test_random_reads ok";
        close ())

    | ["test_random_writes";fn;l;h;n] -> run (
        (* version using plain insert *)
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        let f () = 
          0 |> iter_break (fun i -> 
              match i >= n with 
              | true -> Break()
              | false -> 
                let k = rand ~l ~h in 
                let (k,v) = int2kv k in
                run (insert ~k ~v);
                Cont(i+1))
        in
        measure_execution_time_and_print "test_random_writes" f;
        print_endline "test_random_writes ok";
        close ())

    | ["test_random_writes_im";fn;l;h;n] -> run (
        (* version using insert_many, with sorting and chunks *)
        from_file ~fn ~create ~init >>= fun (fd,close) -> 
        let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls ~note_cached:() fd in
        let l,h,n = int_of_string l, int_of_string h, int_of_string n in
        (* n random writes between >=l and <h *)
        let f () = 
          0 |> iter_break (fun i -> 
              match i >= n with
              | true -> Break()
              | false -> 
                let h' = min (i+chunk_size) n in
                let f _ = rand ~l ~h in
                let kvs = map_range ~f i h' in
                let kvs = List.sort Pervasives.compare kvs in
                let kvs = List.map int2kv kvs in
                run (insert_all ~kvs);
                Cont h')
        in
        measure_execution_time_and_print "test_random_writes_im" f;
        print_endline "test_random_writes_im ok";
        close ())

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


end


let generic_main ~example ~args ~s2k ~s2v ~k2s ~v2s ~i2k ~i2v =
    
    let { map_ops_with_ls; empty_leaf_as_blk; blk_dev_ops; blk_allocator_ref; btree_root_ref; flush_wbc; _ } = example () 

    let create,init = false,false (* defaults *)

    let { from_file; close } = Blk_layer_2.make_from_file_and_close ~monad_ops:imperative_monad_ops ~blk_ops ~empty_leaf_as_blk 

    (* link from_file and close to the example refs *)
    let from_file ~fn ~create ~init =
      from_file ~fn ~create ~init >>= fun (fd,blk_allocator_state,btree_root_state) -> 
      blk_allocator_ref:=blk_allocator_state;
      btree_root_ref:=btree_root_state;
      let close () = 
        run (flush_wbc ~blk_dev_ops:(blk_dev_ops fd) ());
        close ~fd ~blk_allocator_state:(!blk_allocator_ref) ~btree_root_state:(!btree_root_ref) 
      in
      return (fd,close)
    
    let _ = match args with
      | ["init"; fn] -> run (
          from_file ~fn ~create:true ~init:true >>= fun (_fd,close) -> 
          print_endline "init ok";
          close ())

      | ["count"; fn] -> run (
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls ~note_cached:() fd in
          let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
          make_leaf_stream ((!btree_root_ref).btree_root) >>= fun lss -> 
          let count = ref 0 in
          let rec loop lss =
            count:=!count + (List.length (ls_kvs lss));        
            run (ls_step lss) |> function
            | None -> ()
            | Some lss -> loop lss
          in
          loop lss;
          Printf.printf "count ok; %d entries\n%!" !count;
          close ())

      | ["insert";fn;k;v] -> run (
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls ~note_cached:() fd in
          insert ~k:(s2k k) ~v:(s2v v) >>= fun () -> 
          close ())

      | ["delete";fn;k] -> run (
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ delete; _ } = map_ops_with_ls ~note_cached:() fd in
          delete ~k:(s2k k) >>= fun () -> 
          close ())

      | ["list";fn] -> run (
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ leaf_stream_ops; _ } = map_ops_with_ls ~note_cached:() fd in
          let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
          make_leaf_stream ((!btree_root_ref).btree_root) >>= fun lss -> 
          let rec loop lss =
            ls_kvs lss |> (fun kvs -> 
                List.iter
                  (fun (k,v) -> 
                     Printf.printf "%s -> %s\n" (k2s k) (v2s v))
                  kvs;
                run (ls_step lss) |> function
                | None -> ()
                | Some lss -> loop lss)
          in
          loop lss;
          print_endline "list ok";
          close ())

      | ["insert_range";fn;l;h] -> run (
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls ~note_cached:() fd in
          let l,h = int_of_string l, int_of_string h in
          let f () =           
            l |> iter_break (fun l -> 
                match l>=h with 
                | true -> Break ()
                | false -> 
                  let h' = min (l+chunk_size) h in
                  let kvs = List_.map_range ~f:(fun x -> int2kv x) l h' in
                  run (insert_all ~kvs);
                  Cont h')
          in
          measure_execution_time_and_print "insert_range" f;
          print_endline "insert_range ok";
          close ())

      | ["test_random_reads";fn;l;h;n] -> run (
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ find; _ } = map_ops_with_ls ~note_cached:() fd in
          let l,h,n = int_of_string l, int_of_string h, int_of_string n in
          (* n random reads between >=l and <h *)
          let f () = 
            0 |> iter_break (fun i -> 
                match i >= n with 
                | true -> Break()
                | false -> 
                  let k = i2k (rand ~l ~h) in
                  ignore(run (find ~k));
                  Cont(i+1))
          in
          f ();
          print_endline "test_random_reads ok";
          close ())

      | ["test_random_writes";fn;l;h;n] -> run (
          (* version using plain insert *)
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ insert; _ } = map_ops_with_ls ~note_cached:() fd in
          let l,h,n = int_of_string l, int_of_string h, int_of_string n in
          (* n random writes between >=l and <h *)
          let f () = 
            0 |> iter_break (fun i -> 
                match i >= n with 
                | true -> Break()
                | false -> 
                  let k = rand ~l ~h in 
                  let (k,v) = int2kv k in
                  run (insert ~k ~v);
                  Cont(i+1))
          in
          measure_execution_time_and_print "test_random_writes" f;
          print_endline "test_random_writes ok";
          close ())

      | ["test_random_writes_im";fn;l;h;n] -> run (
          (* version using insert_many, with sorting and chunks *)
          from_file ~fn ~create ~init >>= fun (fd,close) -> 
          let Map_ops_with_ls.{ insert_all; _ } = map_ops_with_ls ~note_cached:() fd in
          let l,h,n = int_of_string l, int_of_string h, int_of_string n in
          (* n random writes between >=l and <h *)
          let f () = 
            0 |> iter_break (fun i -> 
                match i >= n with
                | true -> Break()
                | false -> 
                  let h' = min (i+chunk_size) n in
                  let f _ = rand ~l ~h in
                  let kvs = map_range ~f i h' in
                  let kvs = List.sort Pervasives.compare kvs in
                  let kvs = List.map int2kv kvs in
                  run (insert_all ~kvs);
                  Cont h')
          in
          measure_execution_time_and_print "test_random_writes_im" f;
          print_endline "test_random_writes_im ok";
          close ())

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
  in
  ()
*)
*)
