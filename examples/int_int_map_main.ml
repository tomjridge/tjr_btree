(* a map from int to int, backed by file ------------------------------- *)

open Tjr_btree

let k_to_string = string_of_int
let k_of_string = int_of_string
let v_to_string = string_of_int
let v_of_string = int_of_string


let (from_file,close,rest) = Examples.ii_map_on_fd

let main args =
  (* turn off wf checking *)
  Isa_test.disable_isa_checks();
  Test.disable ();
  match args with
  | ["init"; fn] ->
    let state = from_file ~fn ~create:true ~init:true in
    print_endline "init ok";
    close state    

  | ["insert";fn;k;v] -> (
      let ref_ = ref (from_file ~fn ~create:true ~init:false) in
      let (_,insert,_,_) = rest ~ref_ in
      insert (k_of_string k) (v_of_string v);
      close !ref_)

  | ["delete";fn;k] -> (
      let ref_ = ref (from_file ~fn ~create:true ~init:false) in
      let (_,_,delete,_) = rest ~ref_ in
      delete (k_of_string k);
      close !ref_)

  | ["list";fn] -> (
      let ref_ = ref (from_file ~fn ~create:true ~init:false) in
      let (_,_,_,(mk_leaf_stream,ls_step,ls_kvs)) = rest ~ref_ in
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
      let (_,insert,_,_) = rest ~ref_ in
      let l,h = int_of_string l, int_of_string h in
      Tjr_list.from_to l h 
      |> List.iter (fun i -> insert i (2*i));
      close !ref_)

  | ["test_random_reads";fn;l;h;n] -> (
      let ref_ = ref (from_file ~fn ~create:false ~init:false) in
      let (find,_,_,_) = rest ~ref_ in
      let l,h,n = int_of_string l, int_of_string h, int_of_string n in
      (* n random reads between >=l and <h *)
      let d = h - l in
      for _i = 1 to n do
        ignore(find (l+(Random.int d)));
      done;
      print_endline "test_random_reads ok")

  | ["nop"] -> (
      (* print_endline "nop ok" *)
    )

  | _ ->
    failwith (
      Printf.sprintf "Unrecognized args: %s, at %s"
        (String_.concat_strings ~sep:" " args)
        __LOC__)

let _ = main (Sys.argv |> Array.to_list |> List.tl)

