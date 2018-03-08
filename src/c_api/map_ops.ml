(** Map operations: find, insert, [insert_many] and delete. 

[insert_many] attempts to insert as many as possible in a
single operation, and returns the remainder, and so is typically
called in a loop. 

The interfaces are heavily parameterized.  To understand the
interfaces, we need to introduce the following:

- Keys, represented by type variable ['k]
- Values, by type var ['v]
- Page/block references, ['r]; vars [blk_id]
- Global state, ['t]

The operations typically execute in the {!Base_types.Monad}.

*)


open Monad

(* map ------------------------------------------------------------ *)

module Map_ops_type = struct
  type ('k,'v,'t) map_ops = {
    find: 'k -> ('v option,'t) m;
    insert: 'k -> 'v -> (unit,'t) m;
    delete: 'k -> (unit,'t) m;
    insert_many: 'k -> 'v -> ('k*'v) list -> (('k*'v)list,'t) m
  }
end
include Map_ops_type


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
  { find;insert;delete;insert_many }


let dest_map_ops { find;insert;delete;insert_many } =
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  fun k -> k ~find ~insert ~delete ~insert_many

    
(** Utility: call [insert_many] in a loop. *)
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


(* TODO insert_many *)
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


