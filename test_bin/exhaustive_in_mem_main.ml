open Tjr_btree
open Isa_btree.Constants

module Logger = Tjr_fs_shared.Logger

let _ = 
  Logger.logger := Some (Tjr_log.noop_log_ops);
  Logger.at_exit ~print:true;
  begin
    let main' = Test_exhaustive_in_mem.main' ~min:1000 ~max:1024 ~step:2 in 
    Isa_btree.Constants.[cs2312; cs2324; cs2336; cs1112] 
    |> List.iter (fun cs ->
        Printf.printf "\n%s: constants:%s\n%!" 
          __MODULE__ 
          (cs2s cs);
        main' ~constants:cs)
  end;
  Logger.at_exit ~print:false

