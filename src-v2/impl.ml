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
    val constants : constants
    val k_cmp     : k k_cmp
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

  type stack = (r * branch * k option * r * k option) list

  (* NOTE the triple (_,r,leaf) consists of the leaf and the original
     pointer r to the leaf; the first component is a stack: for each
     branch we traverse, we have the original pointer, the branch, and
     the followed pointer with two k bounds *)

  let bounds (stk:stack) = 
    (stk,None,None) |> iter_k (fun ~k x -> match x with
        | ([],lo,hi) -> (lo,hi)
        | ( (_,_,k1,_,k2)::stk,lo,hi) -> 
          (* if we already have a bound, we stick with it, since it is
             tightest *)
          match lo,hi with
          | Some lo, Some hi -> (Some lo,Some hi)
          | None,Some hi     -> k (stk,k1,Some hi)
          | Some lo,None     -> k (stk,Some lo, k2)
          | None,None        -> k (stk,k1,k2) )

  let find ~r ~k = 
    find_leaf ~r ~k >>= fun (_,_,l) -> 
    return (leaf.lookup k l)

  let _ : r:r -> k:k -> (v option, t) m = find

  let {max_leaf_keys;max_branch_keys} = constants

  let leaf_is_large l = 
    leaf.leaf_nkeys l > max_leaf_keys
    
  let split_large_leaf l = 
    leaf.split_leaf max_leaf_keys l

  let branch_is_large b = 
    branch.branch_nkeys b > max_branch_keys

  let split_large_branch b =
    branch.split_branch max_branch_keys b

  let insert ~rebuild ~k ~v ~r =
    find_leaf ~r ~k >>= fun (sofar,r,l) -> 
    leaf.insert k v l;
    match leaf_is_large l with
    | false -> 
      store.write r (node.of_leaf l) >>= fun () -> 
      return (`Ok [])
    | true -> 
      split_large_leaf l |> fun (l1,k,l2) -> 
      alloc () >>= fun r1 -> 
      alloc () >>= fun r2 ->
      store.write r1 (node.of_leaf l1) >>= fun () -> 
      store.write r2 (node.of_leaf l2) >>= fun () -> 
      rebuild ~free:[r] ~sofar ~r1 ~k ~r2 


  (* FIXME shouldn't we always return with a new root? *)
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
      match branch_is_large b with
      | false -> 
        (* FIXME we want to have a new node every point to the root *)
        store.write r3 (node.of_branch b) >>= fun () -> 
        return (`Ok free)
      | true -> 
        (* need to split again *)
        split_large_branch b |> fun (b1,k,b2) -> 
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

  let k_lt,k_leq = (k_lt ~k_cmp, k_leq ~k_cmp)

  (* k1 <= k2 < k3 *)
  let ordered lo k hi =
    k_leq ~k1:lo ~k2:k && 
    k_lt ~k1:k ~k2:hi

  (** Insert multiple kv; once we find a leaf we insert as many as
     possible, rather than repeatedly traversing from the
     root. Assumes kvs ordered by k (or at least, this is needed for
     the performance advantage) *)
  let insert_many ~kvs ~r = 
    match kvs with
    | [] -> return `Unchanged
    | (k,v)::kvs -> 
      find_leaf ~r ~k >>= fun (sofar,r,l) -> 
      let (lo,hi) = bounds sofar in
      (* We insert as many as we can, subject to bounds, upto twice
         the max leaf size; when we can do no more, we either rebuild
         or return directly *)
      let remaining = 
        (k,v)::kvs |> iter_k (fun ~k:kont kvs ->
            match kvs with 
            | [] -> []
            | (k,v)::kvs -> 
              match ordered lo k hi && leaf.leaf_nkeys l < 2*max_leaf_keys with
              | true -> 
                leaf.insert k v l;
                kont kvs
              | false -> kvs)
      in
      match leaf_is_large l with
      | false -> 
        store.write r (node.of_leaf l) >>= fun () -> 
        return (`Remaining remaining)
      | true -> 
        split_large_leaf l |> fun (l1,k,l2) -> 
        alloc () >>= fun r1 -> 
        alloc () >>= fun r2 ->
        store.write r1 (node.of_leaf l1) >>= fun () -> 
        store.write r2 (node.of_leaf l2) >>= fun () -> 
        rebuild ~free:[r] ~sofar ~r1 ~k ~r2 >>= fun x -> 
        return (`Rebuilt (x,`Remaining remaining))

  let _ : 
    kvs:(k * v) list ->
    r:r ->
    ([ `Rebuilt of [ `New_root of r list * r | `Ok of r list ] * [`Remaining of (k * v) list]
     | `Remaining of (k * v) list
     | `Unchanged ],
     t)
      m= insert_many

      
      

  (** For delete, we just delete directly, with no attempt to
      rebalance; so some leaves may well be empty. The bad case for
      this hack is when we insert a huge number of kvs, which we then
      delete. In this case, the tree may be somewhat tall, so lookup
      takes longer than it might if we actually implemented delete
      properly. *)
  let delete ~k ~r =
    find_leaf ~k ~r >>= fun (_,r,l) -> 
    leaf.remove k l;
    store.write r (node.of_leaf l)

end
