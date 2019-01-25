open Tjr_btree
open Tjr_fs_shared

let k_to_string = Small_string.to_string
let k_of_string = Small_string.of_string
let v_to_string = Small_string.to_string
let v_of_string = Small_string.of_string



include struct
  open Examples
  open Internal
  let { from_file; close; rest } = ss_map_on_fd
end

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
      let ref_ = ref (from_file ~fn ~create:false ~init:false) in      
      let ops = (rest ~ref_).imperative_ops in
      ops.insert (k_of_string k) (v_of_string v);
      close !ref_)

  | ["delete";fn;k] -> (
      let ref_ = ref (from_file ~fn ~create:false ~init:false) in
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
      Tjr_list.from_to l h 
      |> List.iter (fun i -> 
          ops.insert 
            (k_of_string (string_of_int i)) 
            (v_of_string (string_of_int (2*i))));
      close !ref_)

  | ["nop"] -> (
      (* print_endline "nop ok" *)
    )

  | _ -> 
    failwith (
      Printf.sprintf "Unrecognized args: %s, at %s"
        (String_.concat_strings ~sep:" " args)
        __LOC__)

let _ = main (Sys.argv |> Array.to_list |> List.tl)
