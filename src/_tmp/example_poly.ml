(* example of poly code without type vars ---------------------------------------- *)

module Poly = struct
  type ('k,'v,'t) ctxt = {
    ord: 'k -> 'k -> int;
    map: (module Map.S with type key = 'k)
  }
end

(*
module Ex  = functor (S: sig 
  type kk
  type vv
  type tt
  (* in order to use Map, we have to provide the ctxt ahead of time,
     which is kind of ugly; better to assume that we somehow can get
     the map from the ctxt itself? *)
  val ctxt: (kk,vv,tt) Poly.ctxt
                      end) -> 
struct

  module OT = struct
    open S
    type t = kk 
    let compare: kk -> kk -> int = ctxt.ord
  end

  module MAP = Map.Make(OT)

  
end
*)

type 'a itself = 'a

module Ex = struct
  type kk
  type vv
  type tt
  type ctxt = (kk,vv,tt) Poly.ctxt

  module type S = Map.S with type key = kk
  (* the carrier type, which is actually derived from the ctxt, and is equal to ctxt.map.t *)
  module MAP : S = (val (Obj.magic ()) : S)
  type t = vv MAP.t

  let find: ctxt -> t -> kk -> vv option = (fun c t k ->
      let module M_ = (val c.map) in
      try 
        Some(M_.find k (Obj.magic t))
      with _ -> None)

  let empty: ctxt -> t = (fun c ->
      let module M_ = (val c.map) in
      Obj.magic (M_.empty))

  let add: ctxt -> t -> kk -> vv -> t = (fun c t k v -> 
      let module M_ = (val c.map) in
      M_.add k v (Obj.magic t) |> Obj.magic)
end



let empty : 'k 'v 't. ('k,'v,'t) Poly.ctxt -> 't = fun ctxt -> 
  let f = Ex.empty (Obj.magic ctxt) in
  Obj.magic f

let find: 'k 'v 't. ('k,'v,'t) Poly.ctxt -> 't -> 'k -> 'v option = fun c t k ->
  Obj.magic(Ex.find (Obj.magic c) (Obj.magic t) (Obj.magic k))

let add: 'k 'v 't. ('k,'v,'t) Poly.ctxt -> 't -> 'k -> 'v -> 't = fun c t k v ->
  Obj.magic(Ex.add (Obj.magic c) (Obj.magic k) (Obj.magic v))

module C_ = struct
  type t
  module M_ = Map.Make(struct 
      type t = int 
      let compare = (Pervasives.compare:int -> int -> int) end)
  let ctxt : (int,int,t) Poly.ctxt = {
    ord = Pervasives.compare;
    map = (module M_)
  }
end

let c = C_.ctxt

let m = empty c
(* let m = add c m 1 2
let _ = find c m 1*)

(* seg fault! but should be possible to get round this 

   perhaps if we have first class modules, we don't even need to do
   things as complicated as this? just make eg empty be:

   fun ctxt -> ctxt.map.empty
*)
