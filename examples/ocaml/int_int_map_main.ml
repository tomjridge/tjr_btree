(* a map from int to int, backed by file ------------------------------- *)
open Tjr_profile.Util.Profiler

open Tjr_btree

let k_to_string = string_of_int
let k_of_string = int_of_string
let v_to_string = string_of_int
let v_of_string = int_of_string

module Internal = Int_int_map_example_functionality.Internal
open Internal

(** FIXME Oseq had some performance bug; this is a hand-rolled version *)
module Seq = struct
  type 'a seq = {
    take: int -> 'a list;
  }
  let ( -- ) = fun l h ->
    let x = ref l in
    { take=
        fun n -> 
          let n = ref n in
          let xs = ref [] in
          while !n > 0 && !x <= h do
            xs:=!x::!xs;
            x:=!x+1;
            n:=!n-1
          done;
          List.rev !xs
    }
  let map f xs = {
    take=fun n -> xs.take n |> List.map f
  }

  let iter f xs = 
    let rec g () = 
      xs.take 1 |> function
      | [] -> ()
      | [x] -> f x; g()
      | _ -> failwith __LOC__
    in
    g ()
end
open Seq

(* for insert_many operations *)
let chunksize = 1000

let insert_seq ~sort ~insert_all ~todo =
  let rec f () = 
    todo.take chunksize |> fun xs ->
    match xs with
    | [] -> ()
    | _ -> 
      let kvs = 
        if sort then List.sort (Pervasives.compare : (int*int) -> (int*int) -> int) xs else xs
      in 
      (profile "jb" @@ fun () -> insert_all kvs);
      f ()
  in
  f ()

(* allow float representation *)
let int_of_string s = 
  float_of_string_opt s |> function
  | None -> int_of_string s
  | Some f -> int_of_float f

let main args =
  Random.self_init ();
  (* turn off wf checking *)
  (* Isa_test.disable_isa_checks(); *)
  (* Test.disable (); *)
  let create,init = false,false in (* defaults *)
  match args with
  | ["init"; fn] ->
    btree_from_file ~fn ~create:true ~init:true |> fun { close; _ } ->
    print_endline "init ok";
    close ()    

  | ["insert";fn;k;v] -> (
      btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
      run (map_ops.insert ~k:(k_of_string k) ~v:(v_of_string v));
      close ())

  | ["delete";fn;k] -> (
      btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
      run (map_ops.delete ~k:(k_of_string k));
      close ())

  | ["list";fn] -> (
      btree_from_file ~fn ~create ~init
      |> fun { run; close; root_block; _ } -> 
      let { make_leaf_stream; ls_step; ls_kvs } = leaf_stream_ops in
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
      let todo = Seq.((l -- h) |> map (fun k -> (k,2*k))) in      
      let insert_all = fun kvs -> run (insert_many ~kvs) in
      insert_seq ~sort:false ~insert_all ~todo;
      close ();
      print_endline "insert_range ok";
    )

  | ["test_random_reads";fn;l;h;n] -> (
      btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random reads between >=l and <h *)
      let d = h - l in
      let todo = Seq.(
          (1--n) 
          |> map (fun _ -> let k = l+(Random.int d) in k))
      in
      Seq.iter (fun k -> ignore(run(map_ops.find ~k))) todo;
      close ();
      print_endline "test_random_reads ok")

  | ["test_random_writes";fn;l;h;n] -> (
      (* version using plain insert *)
      btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random writes between >=l and <h *)
      let d = h - l in
      let todo = Seq.(
          (1--n) 
          |> map (fun _ -> let k = l+(Random.int d) in (k,2*k)))
      in
      Seq.iter (fun (k,v) -> run(map_ops.insert ~k ~v)) todo;
      close ();
      print_endline "test_random_writes ok")

  | ["test_random_writes_im";fn;l;h;n] -> (
      (* version using insert_many, with sorting and chunks *)
      btree_from_file ~fn ~create ~init |> fun { run; close; _ } -> 
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random writes between >=l and <h *)
      let d = h - l in
      let todo = Seq.(
          (1--n) 
          |> map (fun _ -> let k = l+(Random.int d) in (k,2*k)))
      in
      let insert_all = fun kvs -> run (insert_many ~kvs) in
      insert_seq ~sort:true ~insert_all ~todo;
      close ();
      print_endline "test_random_writes_im ok")

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

  | _ ->
    Printf.sprintf "Unrecognized args: %s, at %s"
      (String.concat " " args)
      __LOC__
    |> failwith

(* let _ = main (Sys.argv |> Array.to_list |> List.tl) *)

