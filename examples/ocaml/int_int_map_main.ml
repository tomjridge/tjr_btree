(* a map from int to int, backed by file ------------------------------- *)

open Examples

(* open Examples.Int_int *)
open Generic_main

module S = struct
  include Int_int
  let int_to_k i = i
  let int_to_v i = i
  let k_to_string k = string_of_int k
  let k_of_string s = int_of_string s
  let v_to_string = k_to_string
  let v_of_string = k_of_string
end

include Make_generic_main(S)
