(** A store that works directly with trees, not refs to blks. For
   testing. *)

(* a store that works with trees not refs --------------------------- *)


(* FIXME move to stores? no, this is just for testing with
   exhaustive_in_mem *)

type key = int  [@@deriving yojson]
type value = int  [@@deriving yojson]


type tree = (key,value)Tree.tree [@@deriving yojson]
type r = tree
type frame = (key,value,r)Frame.frame 

let t2s t = t |> tree_to_yojson |> Yojson.Safe.pretty_to_string


module State = struct
  type t = tree
  let compare (x:t) (y:t) = Pervasives.compare x y
end

include struct
  open Tjr_monad.Types
  open Tjr_monad.State_passing
  let monad_ops : State.t state_passing monad_ops = 
    Tjr_monad.State_passing.monad_ops ()
end

let return = monad_ops.return

include struct
  open Store_ops 
  open Frame
  open Tree

  let store_ops =
    let return = monad_ops.return in
    let store_free _rs = return () in
    let store_read r =
      (* r is a tree, but we need to return a frame *)
      let frm =
        match r with
        | Node(ks,ts) -> Disk_node(ks,ts)
        | Leaf(kvs) -> Disk_leaf(kvs)
      in
      return frm
    in
    let store_alloc frm = 
      let node = 
        match frm with
        | Disk_node(ks,ts) -> Node(ks,ts)
        | Disk_leaf(kvs) -> Leaf(kvs)
      in
      return node
    in
    { store_free; store_read; store_alloc }
end



(* sets of trees *)
module Set_ops = Tjr_set.Make(
  struct 
    type t = tree 
    let compare (x:t) (y:t) = Pervasives.compare x y 
  end)


