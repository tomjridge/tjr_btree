(* exhaustive in-mem testing ---------------------------------------- *)


(* we concentrate on relatively small parameters *)

(* example int int btree ---------------------------------------- *)

module Example = struct 
  open Btree
  open In_mem_store
  include In_mem_store.Make(struct 
      module C : CONSTANTS = struct
        let max_leaf_size = 5
        let max_node_keys = 5
        let min_leaf_size = 2
        let min_node_keys = 2
      end

      module KV (* : KEY_VALUE_TYPES *) = struct 
        type key = int[@@deriving yojson]
        type value = int[@@deriving yojson]
        let key_ord k1 k2 = Pervasives.compare k1 k2
        let equal_value = (=)
      end
    end)

  let empty = Map_int.empty

end

(* setup ---------------------------------------- *)

module Map_int = Btree_util.Map_int
open Btree_api
open Example

open Example.Btree
module Tree = Btree.Our_.Tree
module Store = Example.ST



(* state type for testing ---------------------------------------- *)

module Test_state = struct 
    type t = { t:Tree.tree; s:Store.store;r:Store.page_ref }
    (* we want to ignore the store and page_ref *)
    let compare (x:t) (y:t) = (Pervasives.compare (x.t) (y.t))
end
module TS = Test_state

(* for maintaing a set of states *)
module Test_state_set = Set.Make(Test_state)
module TSS = Test_state_set


type action = Insert of int | Delete of int


(* we have a set of states s that we have already generated; 
   we have a set of states todo that we need to process; 
   for each todo, we apply all possible actions; 
   any new states that result are stored in "todo"; *)

(* if we hit an exception, we want to know what the input tree was,
   and what the command was *)


(* FIXME remove *)
let (init_store, init_r) = (
  let open Store in
  let open Our_.Frame_types in
  ({free=1;m=Map_int.empty |> Map_int.add 0 (Leaf_frame[])}, 0)
)

(* save so we know what the last action was *)
let action = ref (Insert 0) 

type range_t = int list[@@deriving yojson]

(* explore all possible states for the given range *)


(* exhaustive testing ---------------------------------------- *)
open Btree_util

let test range = TS.(
    Printf.printf "%s: exhaustive test, %d elts: " 
      __MODULE__ (List.length range);
    flush_out();
    let s = ref TSS.(singleton {t=Tree.Leaf[];s=init_store;r=init_r }) in
    let todo = ref (!s) in
    (* next states from a given tree *)
    let step t = 
      let r1 = (
        range|>List.map (
          fun x -> 
            action:=Insert x; 
            Raw_map.insert x x|>Sem.run (t.s,t.r)))
      in
      let r2 = (
        range|>List.map (
          fun x -> 
            action:=Delete x; 
            Raw_map.delete x|> Sem.run (t.s,t.r)))
      in
      r1@r2 |> List.map (
        fun ((s',r'),res) -> 
          match res with
          | Ok () -> {t=Btree.Our_.Frame.r_to_t s' r'; s=s'; r=r' }
          | Error e -> (failwith (__LOC__^e)))
      |> TSS.of_list
    in
    let _ = 
      (* FIXME this may be faster if we store todo as a list and check
         for membership when computing next state of the head of todo;
         use rev_append *)
      (* Printf.printf "test: starting while\n"; *)
      while (not(TSS.is_empty !todo)) do
        let nexts : TSS.t list = 
          !todo|>TSS.elements|>List.map step in
        let next = List.fold_left 
            (fun a b -> TSS.union a b) 
            TSS.empty nexts 
        in
        let new_ = TSS.diff next !s in
        s:=TSS.union !s new_;
        todo:=new_;
        print_string "."; flush_out ();
        ()
      done
    in
    Printf.printf "tests passed; num states explored: %d\n" (TSS.cardinal !s))


(* testing insert ---------------------------------------- *)

(* do n inserts; check wf *)
let test_insert range = (
  Printf.printf "%s: test_insert, %d inserts, check wf etc:" __MODULE__ (List.length range);
  flush_out();
  let r0 = ref init_r in
  let s0 = ref init_store in
  try (
    let xs = ref range in
    while (!xs <> []) do
      print_string "."; flush_out();
      let x = List.hd !xs in
      let ((s0',r0'),res) = 
        Raw_map.insert x (2*x) |>Sem.run (!s0,!r0) in
      match res with
      | Error e -> (failwith (__LOC__ ^e))
      | Ok () -> 
        s0:=s0';r0:=r0';xs:=List.tl !xs; ()
    done;
    print_newline ();
  ) with _ -> (
      print_endline "Failure...";
      !s0|>ST.store_to_'|>ST.store'_to_yojson
      |>Yojson.Safe.to_string|>print_endline; 
      ()
    )
)


(* testing leaf_stream ---------------------------------------- *)

open Extlib.ExtList.List

type t = int list [@@deriving yojson]

let test_leaf_stream range = (
  Printf.printf "%s: test_leaf_stream, %d inserts, check wf etc:" 
    __MODULE__ 
    (List.length range);
  flush_out();
  let r0 = init_r in
  let s0 = init_store in
  let sr = ref (s0,r0) in
  let run = Sem.run_ref sr in
  (* insert into an empty btree *)
  let xs = ref range in
  while (!xs <> []) do
    print_string "."; flush_out();
    let x = List.hd !xs in
    run (Raw_map.insert x (2*x));
    xs:=tl !xs
  done;
  (* check that leaf stream is what it should be *)
  let open Leaf_stream_ in
  let (s0,r0) = !sr in
  let s0 = ref s0 in
  let ls = Sem.run_ref s0 (Leaf_stream_.mk r0) in
  let run = Sem.run_ref (ref (!s0,ls)) in
  let x = List.sort Pervasives.compare range in
  let y = (run (all_kvs ()) |> List.map fst) in
  Test.log (y|>to_yojson|>Yojson.Safe.to_string);
  assert (x=y);
  print_newline ()
)

