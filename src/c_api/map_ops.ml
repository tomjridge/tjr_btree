(** Map operations: find, insert, [insert_many] and
   delete. [insert_many] attempts to insert as many as possible in a
   single operation, and returns the remainder, and so is typically
   called in a loop. *)

(** This module describes the main interfaces. The interfaces are heavily parameterized. 

To understand the interfaces, we need to introduce the following:

- Keys, represented by type variable ['k]
- Values, by type var ['v]
- Page/block references, ['r]; vars [blk_id]
- Global state, ['t]

The operations typically execute in the {!Base_types.Monad}.

*)





open Monad

(* map ------------------------------------------------------------ *)

(* TODO insert_many *)


(* NOTE insert_many not standard map op *)
let wf_map_ops
    ~(find: 'k -> ('v option,'t) m)
    ~(insert: 'k -> 'v -> (unit,'t) m)
    ~(delete: 'k -> (unit,'t) m)
    ~(insert_many: 'k -> 'v -> ('k*'v) list -> (('k*'v)list,'t) m) 
  = 
  true


let mk_map_ops ~find ~insert ~delete ~insert_many =
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  `Map_ops(find,insert,delete,insert_many)

let dest_map_ops (`Map_ops(find,insert,delete,insert_many)) =
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  fun k -> k ~find ~insert ~delete ~insert_many
    
(** Call [insert_many] in a loop. *)
let rec insert_all insert_many k v kvs = Monad.(
    insert_many k v kvs |> bind (fun kvs' -> 
        match kvs' with
        | [] -> return ()
        | (k,v)::kvs -> insert_all insert_many k v kvs))


let wf_imperative_map_ops
    ~(find: 'k -> 'v option)
    ~(insert: 'k -> 'v -> unit)
    ~(delete: 'k -> unit)
  =
  true


let map_ops_to_imperative map_ops = 
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  fun ~s_ref ->
    let find k = 
      find k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let insert k v = 
      insert k v |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let delete k = 
      delete k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    assert(wf_imperative_map_ops ~find ~insert ~delete);
    `Imperative_map_ops(find,insert,delete)

let dest_imperative_map_ops (`Imperative_map_ops(find,insert,delete)) =
  assert(wf_imperative_map_ops ~find ~insert ~delete);
  fun k -> k ~find ~insert ~delete


(* old -------------------------------------------------------------- *)

(** Dummy module containing imperative version of the map interface. *)
(*
module Imperative_map_ops = struct

  (** Imperative version of the map interface; throws exceptions. WARNING likely to change. *)
  type ('k,'v) imap_ops = {
    find: 'k -> 'v option;
    insert: 'k -> 'v -> unit;
    delete: 'k -> unit;
  }

  open Monad
  let of_map_ops (map_ops:('k,'v,'t)map_ops) (s_ref:'t ref) = (
    let find k = 
      map_ops.find k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let insert k v = 
      map_ops.insert k v |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    let delete k = 
      map_ops.delete k |> run (!s_ref) 
      |> function (s',Ok res) -> (s_ref:=s'; res) | (_,Error e) -> failwith e 
    in
    {find;insert;delete})

  (* FIXME make sure other places where we expect OK actually do
     something with the error *)

end
*)


(*
type ('k,'v,'t) map_opsFIXME = {
(*  find_leaf: 'k -> ( ('k*'v)list,'t) m; not in map api *)
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  insert_many: 'k -> 'v -> ('k*'v) list -> (('k*'v)list,'t) m;
  delete: 'k -> (unit,'t) m;
}
*)
