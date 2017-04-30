open Prelude

let from_to l h : int list = Batteries.(l -- h |> List.of_enum)

let (--) = from_to

type param = Range of (int * int) [@@deriving yojson]

let dest_Range = function Range (l,h) -> (l,h)

type test_t = {
  name: string;
  params: param list;
  with_asserts: bool;
} [@@deriving yojson]

let get_range ps = 
  ps 
  |> List.find (function Range x -> true | _ -> false) 
  |> dest_Range

type tests = test_t list [@@deriving yojson]

module TC = Test_cache
module TMM = Test_mem_map
module TII = Test_int_int_on_fd
module TSI = Test_string_int_on_fd

let tests = [
  (* TODO ("tb", fun ps -> Test_bytestore.(fun () -> main())); *)
  ("tc", fun ps -> TC.main);
  ("tim.t", fun ps -> 
      let (l,h) = get_range ps in
      TMM.(fun() -> test (l -- h)));
  (* TODO ("tim.i", fun ps -> 
      let (l,h) = get_range ps in
      Test_in_mem.(fun () -> test_insert (l -- h |> List.of_enum))); *)
  (* TODO ("tim.ls", fun ps -> 
      let (l,h) = get_range ps in
      Test_in_mem.(fun () -> test_leaf_stream (l -- h |> List.of_enum))); *)
  ("tii.u", fun ps ->
      let (l,h) = get_range ps in
      TII.(fun () -> test_uncached (l -- h)));
  (* TODO ("tii.c", fun ps -> 
      let (l,h) = get_range ps in
      Test_ii.(fun () -> 
      test_cached (l -- h |> List.of_enum))); *)
  ("tsi", fun ps -> TSI.test);
]

(* FIXME also main2 *)

let run_test t = (
  match (try Some(List.assoc t.name tests) with _ -> None) with
  | None -> (
      Test.warn (Printf.sprintf "%s: unknown test %s\n" __MODULE__ t.name))
  | Some test -> (  
      let _ = if t.with_asserts then Test.enable() else Test.disable() in
      test t.params ()
    ))

let _ = try (
  match Array.to_list Sys.argv |> List.tl with
  (* run tests based on json config file *)
  | [n] -> (
      let s = read_file n in
      let Ok tests = s|>Yojson.Safe.from_string|>tests_of_yojson in
      List.iter (fun t -> run_test t) tests
    )
) with e -> (
    Test.print_logs ();
    e|>Printexc.to_string|>print_endline;
    Printexc.get_backtrace () |>print_endline;
    flush_out();
  )






(*
let _ = 
    {name="axx"; params=[Range(1,10)]; with_asserts=true;}
    |>test_t_to_yojson
    |>Yojson.Safe.pretty_to_string
    |>print_endline
*)


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
