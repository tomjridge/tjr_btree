open Base_types

(* test config ------------------------------------------------------ *)

let fields = [
  "range_min";
  "range_max";
  "range_step";
  "min_leaf_size";
  "max_leaf_size";
  "min_node_keys";
  "max_node_keys"
]


type config = (string * int) list [@@deriving yojson]


let get (c:config) (f:string) =
  List.assoc f c  (* throw exception if not found *)


let wf_config c =
  fields|>List.iter (fun f -> ignore(get c f));
  true



  

(* note --------------------------------------------------------- *)

(*

The aim is to test the operations at the ADT level. For this, we need
to instantiate ('k,'v,'r,'t) store_ops with appropriate 'r and 't. 

Previous testing (mem_store) used 

type ('k,'v) mem = {free:int; map:('k,'v)frame Map_int.t}  

We instead choose 'r = ('k,'v) tree and 't = ().


---

Starting with the empty tree, we apply every insert/delete action
possible. We check invariants before and after the action. We also
check intermediate states. The aim is to identify:

- a particular wellformed state
- an action
- the sequence of small steps involved in executing the action (we can
  log all small steps)
- a resulting malformed state
- the clause of a wellformedness property which is broken (implemented
  via exception line number)

We then pretty-print these states.

---

For insert (say) we want to test that the tree at start and end is
wellformed, and that the corresponding map is what it should be. To
avoid recalculating the map, we store it with the tree, so

'r = ('k,'v) map * ('k,'v) tree

Note that OCaml maps require Map.equal for comparison

*)


open Insert_delete_ops

(* because of the min/max parameters, the step function is really a
   runtime thing, so the functorization isn't helping here *)

open Tree_store

let set_ops = Set_ops.set_ops


(* FIXME need to add wellformedness checks on the following *)

let run = Tjr_step_monad.Extra.run

let execute_tests ~constants ~map_ops ~ops ~init_trees = 
  let find,insert,delete = 
    Map_ops.dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many -> 
    (find,insert,delete)
  in

  let step t op = 
    (* print_endline __LOC__; *)
    Test.log(fun _ -> Printf.sprintf "%s: about to perform action %s on %s" 
                __LOC__ (op2s op) (t2s t));
    match op with
    | Insert i -> 
      Test.log (fun _ -> Printf.sprintf "%s: inserting %d" __LOC__ i);
      insert i i |> run t
    | Delete i ->
      Test.log (fun _ -> Printf.sprintf "%s: deleting %d" __LOC__ i);
      delete i |> run t
  in

  let check_state t = assert(
    Test.log(fun _ -> Printf.sprintf "%s: checking invariants on %s" 
                __LOC__ (t2s t));    
    (* FIXME shouldn't there be a wellformed tree with default maybe_small? *)
    Tree.wellformed_tree ~constants 
      ~maybe_small:(Some Isa_export.Prelude.Small_root_node_or_leaf)
      ~cmp:Int_.compare
      t) 
  in

  (* step should return a list of next states *)
  let step t op = 
    step t op |> fun (t,()) -> 
    Test.log (fun _ -> Printf.sprintf "%s: result of op %s was %s" 
                 __LOC__ (op2s op) (t2s t));
    check_state t;
    [t]
  in
  (* FIXME remove check from exhaustive - just do it in the step phase *)

  let check_step t1 t2 = () in

  let test_ops = Exhaustive.{ step; check_state; check_step } in
  
  let test = Exhaustive.test ~set_ops ~test_ops in
  test ops init_trees

let _ = execute_tests

let page_ref_ops = Tjr_step_monad.Step_monad_implementation.{
    get=(fun () -> with_world (fun t -> (t,t)));
    set=(fun r -> with_world (fun t -> ((),r)));
  }


let main' ~min ~max ~step ~constants = 
  let range = Range.mk_range ~min ~max ~step in
  let ops = 
    range|>List.map (fun x -> Insert x) |> fun xs ->
    range|>List.map (fun x -> Delete x) |> fun ys -> xs@ys
  in
  let ps = 
    object
      method cmp=Int_.compare
      method constants=constants
    end
  in
  let map_ops = Store_to_map.store_ops_to_map_ops ~ps ~page_ref_ops ~store_ops in
  execute_tests ~constants ~map_ops ~ops ~init_trees:[Tree.Leaf[]]

let _ = main'


let main c = 
  let [min;max;step;l;l';m;m'] = List.map (fun f -> get c f) [
      "range_min";
      "range_max";
      "range_step";
      "min_leaf_size";
      "max_leaf_size";
      "min_node_keys";
      "max_node_keys"
    ]
  in
  let constants = Constants.{
      min_leaf_size=l; max_leaf_size=l';
      min_node_keys=m; max_node_keys=m' }
  in
  main' ~min ~max ~step ~constants


