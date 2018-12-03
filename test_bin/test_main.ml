open Tjr_btree

let from_to l h : int list = Tjr_list.from_to l h

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

(* module TC = Test_cache *)
module TMM = Test_mem_map
module TII = Test_int_int_on_fd
module TSI = Test_string_int_on_fd

let tests = [
  (* TODO ("tb", fun ps -> Test_bytestore.(fun () -> main())); *)
  (* ("test_cache", fun ps -> 
   *     let (l,h) = get_range ps in
   *     TC.test (l--h)); *)
  ("test_exhaustive", fun ps -> 
      let (l,h) = get_range ps in
      TMM.test_exhaustive (l -- h));
  ("test_insert", fun ps -> 
      let (l,h) = get_range ps in
      TMM.test_insert (l -- h));
  (* TODO ("tim.ls", fun ps -> 
      let (l,h) = get_range ps in
      Test_in_mem.(fun () -> test_leaf_stream (l -- h |> List.of_enum))); *)
  ("tii.uncached", fun ps ->
      let (l,h) = get_range ps in
      TII.test_uncached (l -- h));
  (* TODO ("tii.c", fun ps -> 
      let (l,h) = get_range ps in
      Test_ii.(fun () -> 
      test_cached (l -- h |> List.of_enum))); *)
  ("tsi", fun ps -> 
      TSI.test ());
]

(* FIXME also main2 *)

let run_test t = (
  match (try Some(List.assoc t.name tests) with _ -> None) with
  | None -> (
      Test.warn (Printf.sprintf "%s: unknown test %s\n" __MODULE__ t.name))
  | Some test -> (  
      let _ = if t.with_asserts then Test.enable() else Test.disable() in
      test t.params
    ))

let _ = 
(* FIXME we seem to have to comment out the try if we want to get decent backtraces *)
(* FIXME add printing as an exit hook *)
  Pervasives.at_exit (
    fun () -> 
      print_endline (__LOC__ ^ ": running exit hooks");
      Test.run_exit_hooks ());
(*  try ( *)
    match Array.to_list Sys.argv |> List.tl with
    (* run tests based on json config file *)
    | [n] -> (
        let s = Tjr_fs_shared.File_util.read_file n in
        let Ok tests = s|>Yojson.Safe.from_string|>tests_of_yojson in
        List.iter (fun t -> run_test t) tests
      )
(*  ) with e -> (
      Test.print_logs ();
      Base_types.flush_out();
      print_endline "(";
      ignore(e|>Printexc.to_string|>print_endline);
      print_endline ")";
      Base_types.flush_out();
      print_endline "(";
      ignore(Printexc.get_backtrace () |>print_endline);
      print_endline ")";
      Base_types.flush_out();
      raise e
    )
*)





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
