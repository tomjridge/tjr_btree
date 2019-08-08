open Examples
open Generic_main

let example = Imperative.int_int_example

let s2k = int_of_string
let s2v = int_of_string
let k2s = string_of_int
let v2s = string_of_int
let i2k = fun x -> x
let i2v = fun x -> x

let main ~args = generic_main ~example ~args ~s2k ~s2v ~k2s ~v2s ~i2k ~i2v

