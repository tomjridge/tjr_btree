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


open Tjr_monad.Types

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
  true[@@ocaml.warning "-27"]


let mk_map_ops ~find ~insert ~delete ~insert_many =
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  { find;insert;delete;insert_many }


let dest_map_ops { find;insert;delete;insert_many } =
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  fun k -> k ~find ~insert ~delete ~insert_many

    
(** Utility: call [insert_many] in a loop. *)
let insert_all ~monad_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  fun insert_many ->
    let rec loop k v kvs =
    insert_many k v kvs >>= (fun kvs' -> 
        match kvs' with
        | [] -> return ()
        | (k,v)::kvs -> loop k v kvs)
    in loop


let wf_imperative_map_ops
    ~(find: 'k -> 'v option)
    ~(insert: 'k -> 'v -> unit)
    ~(delete: 'k -> unit)
  =
  true[@@ocaml.warning "-27"]


let run = Tjr_monad.State_passing.run

(* TODO insert_many *)
let state_passing_map_ops_to_imperative map_ops = 
  dest_map_ops map_ops @@ fun ~find ~insert ~delete ~insert_many ->
  assert(wf_map_ops ~find ~insert ~delete ~insert_many);
  fun ~s_ref ->
    let find k = 
      find k |> run ~init_state:!s_ref
      |> function (res,s') -> (s_ref:=s'; res) 
    in
    let insert k v = 
      insert k v |> run ~init_state:!s_ref
      |> function (res,s') -> (s_ref:=s'; res) 
    in
    let delete k = 
      delete k |> run ~init_state:!s_ref
      |> function (res,s') -> (s_ref:=s'; res)
    in
    assert(wf_imperative_map_ops ~find ~insert ~delete);
    `Imperative_map_ops(find,insert,delete)


let dest_imperative_map_ops (`Imperative_map_ops(find,insert,delete)) =
  assert(wf_imperative_map_ops ~find ~insert ~delete);
  fun k -> k ~find ~insert ~delete

