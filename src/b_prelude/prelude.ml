(* prelude ---------------------------------------- *)

(* collect together modules, then open Prelude at top of following *)

module Btree_util = Btree_util
module Functional_store = Functional_store
module Lens = Lens.Lens
module Pickle = Pickle
module Mut = State_error_monad.Mut
module Sem = State_error_monad.Sem
module State_error_monad = State_error_monad.State_error_monad
module Tjr_string = Tjr_string
module Test = Test
module World = World_monad.World

module Simple_monad = struct
  type ('a,'s) m = 's -> 's * ('a,string) result
  let bind : ('a -> ('b,'s) m) -> ('a,'s)m -> ('b,'s)m = fun f x s ->
    match x s with
    | (s',Ok y) -> f y s'
    | (s',Error e) -> (s',Error e)
  let return x = fun s -> (s,Ok x)
end

module Bt_pervasives = struct
  module Int = struct
    type t = int 
    let compare: t -> t -> int = Pervasives.compare 
  end
  module Set_int = Set.Make(Int)
  module Map_int = Map.Make(Int)
  module Map_string = Map.Make(String)

  let impossible (x:string) = failwith ("impossible: "^x)

  let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

  let option_map f = function Some x -> Some(f x) | _ -> None
end

include Bt_pervasives
