(* extra stuff after running test.sh *)

let _ = Test.init_r

open Test.BT

let _ = Test.test [0;1;2;3;4;5;6;7;8;9]

(* FIXME add to_string funs to Delete; FIXME don't need outer Some *)
let Some(s,s') = !Delete.last_trans 

let _ = 
  s.Delete.ds 
  |> M.Delete.d_state_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline

let _ = 
  s'.Delete.ds
  |> M.Delete.d_state_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline


let _ = 
  s.Delete.t
  |> M.Tree.tree_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline

let _ = 
  s'.Delete.t
  |> M.Tree.tree_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline



let _ = Delete.from_store s.store s

let _ = Delete.from_store s'.store s'



  |> M.Tree.tree_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline
