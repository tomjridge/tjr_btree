(* a store that works with trees not refs --------------------------- *)


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
  open Monad
  open Store_ops 
  open Frame
  open Tree
  let store_ops = {
    store_free=(fun rs -> return ());
    store_read=(fun r -> 
        (* r is a tree, but we need to return a frame *)
        return @@
        match r with
        | Node(ks,ts) -> Disk_node(ks,ts)
        | Leaf(kvs) -> Disk_leaf(kvs));                     
    store_alloc=(fun frm -> 
        return @@ 
        match frm with
        | Disk_node(ks,ts) -> Node(ks,ts)
        | Disk_leaf(kvs) -> Leaf(kvs));
  }
end



(* sets of trees *)

module Set_ops = Exhaustive.Make_set_ops(
  struct type t = tree let compare (x:t) (y:t) = Pervasives.compare x y end)


