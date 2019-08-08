(** A simple example of a kv store. *)

open Examples

let example = Imperative.int_int_example ()

let int_to_k = fun x -> x
let int_to_v = fun x -> x

let main ~fn = Generic_example.make_generic_main ~fn ~int_to_k
    ~int_to_v ~example |> Tjr_monad.Imperative.of_m
