open Tjr_btree
open Isa_btree.Constants

let _ = 
  Test.add_exit_hook (Test.print_logs);
  Pervasives.at_exit @@ fun () -> 
  print_endline (__LOC__ ^ ": running exit hooks");
  Test.run_exit_hooks ()


let _ = 
  let main' = Test_exhaustive_in_mem.main' ~min:1000 ~max:1024 ~step:2 in
  Isa_btree.Constants.[cs2312; cs2324; cs2336; cs1112] |> List.iter (fun cs ->
      Printf.sprintf "%s: constants:%s\n" __LOC__ (cs2s cs)|>print_endline;
      main' ~constants:cs)
