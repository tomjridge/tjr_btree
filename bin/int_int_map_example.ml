(** A simple example of a kv store. *)

open Examples
open Examples.Imperative

let example = Imperative.int_int_example

let int_to_k = fun x -> x
let int_to_v = fun x -> x

let main ~fn = 
  let Examples.{monad_ops;blk_ops;empty_leaf_as_blk; _} = example in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let from_file_and_close = Blk_layer.make_from_file_and_close ~monad_ops ~blk_ops ~empty_leaf_as_blk in
  from_file_and_close.from_file ~fn ~create:true ~init:true >>= fun (fd,rb) -> 
  let run = Generic_example.{run=Tjr_monad.Imperative.of_m} in
  let { free; btree_root } = rb in
  let free = ref free in
  let root = ref btree_root in
  let btree_root_ops = Btree_root_ops(with_imperative_ref ~monad_ops root) in
  let map_ops_with_ls = example.map_ops_with_ls fd btree_root_ops in
  let example = Generic_example.make_generic_example
      ~map_ops_with_ls
      ~run 
      ~int_to_k ~int_to_v
  in
  example.do_all ();
  (* FIXME switching between monad and imperative is a bit odd *)
  return ()
