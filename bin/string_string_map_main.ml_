open Examples

open Generic_main

module S = struct
  include Ss_ss
  let int_to_k i = i |> string_of_int |> Small_string.of_string
  let int_to_v i = i |> string_of_int |> Small_string.of_string
  let k_to_string k = k |> Small_string.to_string
  let k_of_string s = s |> Small_string.of_string
  let v_to_string = k_to_string
  let v_of_string = k_of_string
end

include Make_generic_main(S)
