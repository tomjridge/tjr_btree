open Btree_util

type param = Range of (int * int) [@@deriving yojson]

let dest_Range = function Range (l,h) -> (l,h)

type test_t = {
  name: string;
  params: param list;
  with_asserts: bool;
} [@@deriving yojson]

let get_range ps = 
  ps |> List.find (function Range x -> true | _ -> false) 
  |> dest_Range

type tests = test_t list [@@deriving yojson]

(*
let _ = 
    {name="axx"; params=[Range(1,10)]; with_asserts=true;}
    |>test_t_to_yojson
    |>Yojson.Safe.pretty_to_string
    |>print_endline
*)

(*
module Test_map = Map.Make(struct 
    type t = string 
    let compare: string -> string -> int = Pervasives.compare
end)
*)

let tests = [
  ("tb", fun ps -> Test_bytestore.(fun () -> main()));
  ("tc", fun ps -> Test_cache.main);
  ("tim.t", fun ps -> 
      let (l,h) = get_range ps in
      Test_in_mem.(fun() -> test Batteries.(l -- h |> List.of_enum)));
  ("tim.i", fun ps -> 
      let (l,h) = get_range ps in
      Test_in_mem.(fun () -> test_insert Batteries.(l -- h |> List.of_enum)));
  ("tim.ls", fun ps -> 
      let (l,h) = get_range ps in
      Test_in_mem.(fun () -> test_leaf_stream Batteries.(l -- h |> List.of_enum)));
  ("tii.u", fun ps ->
      let (l,h) = get_range ps in
      Test_ii.(fun () -> 
      test_uncached Batteries.(l -- h |> List.of_enum)));
  ("tii.c", fun ps -> 
      let (l,h) = get_range ps in
      Test_ii.(fun () -> 
      test_cached Batteries.(l -- h |> List.of_enum)));
  ("tsi", fun ps -> Test_string_int.test);
]

(* FIXME also main2 *)

(*
let main () = (  
    (* read stdin and convert to an int list range *)
    let _ = Printf.printf "test: reading input from stdin\n" in
    let js = Yojson.Safe.from_channel Pervasives.stdin in
    let _ = Printf.printf "test: read %s\n" (Yojson.Safe.to_string js) in
    let range = range_t_of_yojson js |> function Ok x -> x in
    test range
)
*)

let run_test t = (
  let test = List.assoc t.name tests in
  let _ = if t.with_asserts then Test.enable() else Test.disable() in
  test t.params ()
)


let _ = try (
  match Array.to_list Sys.argv |> List.tl with
  (* run tests based on json config file *)
  | [n] -> (
      let s = Btree_util.read_file n in
      let Ok tests = s|>Yojson.Safe.from_string|>tests_of_yojson in
      List.iter (fun t -> run_test t) tests
    )
) with e -> (
    Test.print_logs ();
    e|>Printexc.to_string|>print_endline;
    Printexc.get_backtrace () |>print_endline;
    flush_out();
  )
