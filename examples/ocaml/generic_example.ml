(** A simple example of a kv store. *)

(* open Examples *)
(* open Fstore_layer *)
open Blk_layer

let fn = 
  ref "btree.store"
  |> Global.register ~name:(__MODULE__^".fn (default: btree.store)")

module type T = sig
  val do_write : unit -> unit
  val do_delete : unit -> unit
  val do_check : unit -> unit
  val do_full_check : unit -> unit
  val do_all : unit -> unit
end

let profile s f = Tjr_profile_with_core.time_function s f

let make_generic_example (type k v r leaf_stream) 
    ~btree_from_file
    ~(map_ops_etc: (k,v,r,leaf_stream,fstore state_passing)Btree_intf.Map_ops_etc_type.map_ops_etc)
    ~int_to_k ~int_to_v
  =
  let {btree_from_file} = btree_from_file in

  (* FIXME config *)
  let max_writes = 10000 in

  let module A = struct
    (* Some examples *)

    (* create and init store, write some values, and close *)
    let do_write () = profile "write" @@ fun () -> 
      Printf.printf "Executing %d writes...\n%!" max_writes;
      print_endline "Writing...";
      btree_from_file ~fn:!fn ~create:true ~init:true |> fun { run; close; _ } -> 
      (* write values *)
      for x=1 to max_writes do
        let k,v = int_to_k x,int_to_v x in
        run (map_ops_etc.insert ~k ~v)
      done;
      close ();
      ()

    let _ = do_write

    (* open store, delete some values, and close *)
    let do_delete () = profile "del" @@ fun () -> 
      print_endline "Deleting...";
      btree_from_file ~fn:!fn ~create:false ~init:false |> fun { run; close; _ } -> 
      for x=100 to 200 do
        let k = int_to_k x in
        run (map_ops_etc.delete ~k);
      done;
      close ();
      ()

    (* open store and check whether various keys and values are correct *)
    let do_check () = 
      print_endline "Checking...";
      btree_from_file ~fn:!fn ~create:false ~init:false |> fun { run; close; _ } -> 
      assert(run (map_ops_etc.find ~k:(int_to_k 100)) = None);
      assert(run (map_ops_etc.find ~k:(int_to_k 1000)) = Some (int_to_v 1000));
      close ();
      ()
      
    let do_full_check () = profile "full" @@ fun () -> 
      print_endline "Full check...";
      btree_from_file ~fn:!fn ~create:false ~init:false |> fun { run; close; _ } -> 
      for i = 1 to max_writes do
        let k = int_to_k i in
        if (100 <= i && i <= 200) then
          assert(run (map_ops_etc.find ~k) = None)
        else
          assert(run (map_ops_etc.find ~k) = Some(int_to_v i))
      done;
      close ();
      ()

    (* actually execute the above *)
    let do_all() = 
      do_write ();
      do_delete ();
      do_check ();
      do_full_check()

      
  end
  in
  (module A : T)

let _ = make_generic_example
