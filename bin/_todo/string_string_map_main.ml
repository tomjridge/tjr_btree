open Examples
open Generic_main

let example = Imperative.ss_ss_example

let s2k = Small_string.of_string
let s2v = Small_string.of_string
let k2s = Small_string.to_string
let v2s = Small_string.to_string
let i2k i = i |> string_of_int |> s2k
let i2v i = i |> string_of_int |> s2v

let main ~args = generic_main ~example ~args ~s2k ~s2v ~k2s ~v2s ~i2k ~i2v
