(* a map from int to int, backed by file ------------------------------- *)

open Tjr_btree

let k_to_string = string_of_int
let k_of_string = int_of_string
let v_to_string = string_of_int
let v_of_string = int_of_string

include struct
  open Examples
  open Internal
  let { from_file; close; rest } = ii_map_on_fd
end

(* for insert_many operations *)
let chunksize = 1000

let insert_seq ~insert_all ~todo =
  let todo = ref todo in
  while !todo () <> Seq.Nil do
    let kvs = 
      (OSeq.take chunksize !todo) 
      |> OSeq.to_list 
    in 
    insert_all kvs;
    todo := OSeq.drop chunksize !todo
  done


let main args =
  (* turn off wf checking *)
  Isa_test.disable_isa_checks();
  Test.disable ();
  let open Examples.Internal in
  match args with
  | ["init"; fn] ->
    let state = from_file ~fn ~create:true ~init:true in
    print_endline "init ok";
    close state    

  | ["insert";fn;k;v] -> (
      let ref_ = ref (from_file ~fn ~create:true ~init:false) in
      let ops = (rest ~ref_).imperative_ops in
      ops.insert (k_of_string k) (v_of_string v);
      close !ref_)

  | ["delete";fn;k] -> (
      let ref_ = ref (from_file ~fn ~create:true ~init:false) in
      let ops = (rest ~ref_).imperative_ops in
      ops.delete (k_of_string k);
      close !ref_)

  | ["list";fn] -> (
      let ref_ = ref (from_file ~fn ~create:true ~init:false) in
      let (mk_leaf_stream,ls_step,ls_kvs) = (rest ~ref_).leaf_stream_ops in
      mk_leaf_stream () |> fun lss ->
      let rec loop lss =
        match lss with
        | None -> ()
        | Some lss -> 
          let _ = 
            List.iter 
              (fun (k,v) -> 
                 Printf.printf "%s -> %s\n" (k_to_string k) (v_to_string v))
              (ls_kvs lss)
          in
          loop (ls_step lss)
      in
      loop (Some lss);
      close !ref_;
      print_endline "list ok")

  | ["insert_range";fn;l;h] -> (
      let ref_ = ref (from_file ~fn ~create:false ~init:false) in
      let ops = (rest ~ref_).imperative_ops in
      let l,h = int_of_string l, int_of_string h in
      let todo = OSeq.((l -- h) |> map (fun k -> (k,2*k))) in      
      insert_seq ~insert_all:ops.insert_all ~todo;
      close !ref_;
      print_endline "insert_range ok")

  | ["test_random_reads";fn;l;h;n] -> (
      let ref_ = ref (from_file ~fn ~create:false ~init:false) in
      let ops = (rest ~ref_).imperative_ops in
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random reads between >=l and <h *)
      let d = h - l in
      let todo = OSeq.(
          (1--n) 
          |> map (fun _ -> let k = l+(Random.int d) in k))
      in
      Seq.iter (fun k -> ignore(ops.find k)) todo;
      close !ref_;
      print_endline "test_random_reads ok")

  | ["test_random_writes";fn;l;h;n] -> (
      let ref_ = ref (from_file ~fn ~create:false ~init:false) in
      let ops = (rest ~ref_).imperative_ops in
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random writes between >=l and <h *)
      let d = h - l in
      let todo = OSeq.(
          (1--n) 
          |> map (fun _ -> let k = l+(Random.int d) in (k,2*k)))
      in
      insert_seq ~insert_all:ops.insert_all ~todo;
      close !ref_;
      print_endline "test_random_writes ok")

  | ["nop"] -> (
      (* print_endline "nop ok" *)
    )

  | _ ->
    failwith (
      Printf.sprintf "Unrecognized args: %s, at %s"
        (String_.concat_strings ~sep:" " args)
        __LOC__)

let _ = main (Sys.argv |> Array.to_list |> List.tl)

