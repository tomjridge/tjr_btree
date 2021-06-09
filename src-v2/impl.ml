(** B-tree implementation *)

open Intf

module Make(S:sig
    type leaf
    type branch
    type node
    type k
    type v
    type r
    type t
    val leaf      : (k,v,leaf)leaf_ops
    val branch    : (k,r,branch)branch_ops
    val node      : (branch,leaf,node)node_ops
    val store     : (r,node,t)store_ops
    val alloc     : unit -> ('r,'t)m
    val monad_ops : t monad_ops
  end) = struct
  open S

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return 

  let find_leaf ~r ~k:k0 = 
    let rec find_r ~sofar ~r =
      store.read r >>= fun n -> 
      find_n ~sofar ~r ~n
    and find_n ~sofar ~r ~n =
      node.cases n
        ~leaf:(fun l -> return (sofar,r,l))
        ~branch:(fun b -> 
            let (k1,r1,k2) = branch.find k0 b in
            find_r ~sofar:( (r,b,k1,r1,k2)::sofar ) ~r)
    in
    find_r ~sofar:[] ~r

  let _ : r:r -> k:k -> ((r * branch * k option * r * k option) list * r * leaf, t) m = find_leaf

  let find ~r ~k = 
    find_leaf ~r ~k >>= fun (_,_,l) -> 
    return (leaf.lookup k l)

  let _ : r:r -> k:k -> (v option, t) m = find

  let insert ~rebuild ~k ~v ~r =
    find_leaf ~r ~k >>= fun (sofar,r,l) -> 
    leaf.insert k v l;
    match leaf.is_large l with
    | false -> 
      store.write r (node.of_leaf l) >>= fun () -> 
      return (`Ok [])
    | true -> 
      leaf.split_large_leaf l |> fun (l1,k,l2) -> 
      alloc () >>= fun r1 -> 
      alloc () >>= fun r2 ->
      store.write r1 (node.of_leaf l1) >>= fun () -> 
      store.write r2 (node.of_leaf l2) >>= fun () -> 
      rebuild ~free:[r] ~sofar ~r1 ~k ~r2 

  let rec rebuild :    
    free:r list ->
    sofar:(r * branch * k option * r * k option) list ->
    r1:r ->
    k:k ->
    r2:r ->
    _
    = 
    fun ~free ~sofar ~r1 ~k ~r2 -> 
    match sofar with 
    | [] -> 
      (* need a new root *)
      alloc () >>= fun r -> 
      branch.make_small_root (r1,k,r2) |> fun b -> 
      store.write r (node.of_branch b) >>= fun () -> 
      return (`New_root(free,r))
    | (r3,b,k1,r4,k2)::sofar -> 
      branch.replace (k1,r4,[],k2) (k1,r1,[(k,r2)],k2) b;
      match branch.is_large b with
      | false -> 
        store.write r3 (node.of_branch b) >>= fun () -> 
        return (`Ok free)
      | true -> 
        (* need to split again *)
        branch.split_large_branch b |> fun (b1,k,b2) -> 
        alloc () >>= fun r1 -> 
        alloc () >>= fun r2 ->
        store.write r1 (node.of_branch b1) >>= fun () -> 
        store.write r2 (node.of_branch b2) >>= fun () -> 
        rebuild ~free:(r3::free) ~sofar ~r1 ~k ~r2

  let _ : free:r list ->
    sofar:(r * branch * k option * r * k option) list ->
    r1:r -> k:k -> r2:r -> ([> `New_root of r list * r | `Ok of r list ], t) m 
    = rebuild

  let insert = insert ~rebuild

  let _ : k:k -> v:v -> r:r -> ([ `New_root of r list * r | `Ok of r list], t) m = insert

  (** For delete, we just delete directly, with no attempt to
      rebalance; so some leaves may well be empty. The bad case for
      this hack is when we insert a huge number of kvs, which we then
      delete. In this case, the tree may be somewhat tall, but so
      lookup takes longer than it might. *)
  let delete ~k ~r =
    find_leaf ~k ~r >>= fun (_,r,l) -> 
    leaf.remove k l;
    store.write r (node.of_leaf l)

end
