(* example of poly code without type vars ---------------------------------------- *)


module Poly = struct
  type ('k,'v,'t) ctxt = {
    ord: 'k -> 'k -> int;
    empty: 't;
    find: 'k -> 't -> 'v;
    add: 'k -> 'v -> 't -> 't;
  }
end

open Poly 

let empty : ('k,'v,'t) ctxt -> 't  = (
  fun c -> c.empty)

let find = fun c -> c.find

let add = fun c -> c.add


module C = struct
  module M = Map.Make(struct 
      type t = int 
      let compare = (Pervasives.compare:int -> int -> int) 
    end)
  let ctxt : (int,int,int M.t) Poly.ctxt = {
    ord = Pervasives.compare;
    empty = M.empty;
    find = M.find;
    add = M.add;
  }
end

let c = C.ctxt

let m = empty c
let m = add c 1 2 m
let _ = find c 1 m


