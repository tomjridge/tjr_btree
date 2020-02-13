(** A simple example of a kv store; k=string, v=string. *)
open Examples

let example = Imperative.ss_ss_example ()

let int_to_k = fun x -> x |> string_of_int |> Small_string.of_string
let int_to_v = fun x -> x |> string_of_int |> Small_string.of_string

let main ~fn = Generic_example.make_generic_main ~fn ~int_to_k 
    ~int_to_v ~example |> Tjr_monad.Imperative.of_m
