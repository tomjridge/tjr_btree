let string_of_chars chars = chars|>List.map (String.make 1)|>String.concat ""

type any_t

let any_ref = ref ((Obj.magic 1):any_t)

open Gen_isa

module Util : sig
  type error_t = String_error of string
  type ('a, 'b) rresult = Ok of 'a | Error of 'b
  val rev_apply : 'a -> ('a -> 'b) -> 'b
  val unzip : ('a * 'b) list -> 'a list * 'b list
  val failwitha : string -> 'a
  val split_at : Arith.nat -> 'a list -> 'a list * 'a list
  val dest_list : 'a list -> 'a * 'a list
  val iter_step : ('a -> 'a option) -> 'a -> 'a
  val dest_lista : 'a list -> 'a list * 'a
  val split_at_3 : Arith.nat -> 'a list -> 'a list * ('a * 'a list)
  val assert_true : 'a -> bool -> bool
  val impossible1 : string -> 'a
  val assert_truea : bool -> bool
end = struct

type error_t = String_error of string;;

type ('a, 'b) rresult = Ok of 'a | Error of 'b;;

let rec rev_apply x f = f x;;

let rec unzip
  xs = (rev_apply xs (List.map Product_Type.fst),
         rev_apply xs (List.map Product_Type.snd));;

let rec failwitha x = x|>failwith;;

let rec split_at n xs = (List.take n xs, List.drop n xs);;

let rec dest_list
  xs = (match xs with [] -> failwitha "dest_list" | a :: b -> (a, b));;

let rec iter_step
  f x = let a = f x in
        (match a with None -> x | Some aa -> iter_step f aa);;

let rec dest_lista
  xs = (match xs with [] -> failwitha "dest_list\039 "
         | _ :: _ -> (List.butlast xs, List.last xs));;

let rec split_at_3
  n xs =
    (List.take n xs,
      (List.nth xs n, List.drop (Arith.plus_nat n Arith.one_nat) xs));;

let rec assert_true arg b = (
  let _ = any_ref := ((Obj.magic arg):any_t) in
  if b then b else failwith "assert_true")
;;

let rec impossible1 x = failwitha "";;

let rec assert_truea b = assert_true () b;;

end;;

module Prelude : sig
  val from_to : Arith.nat -> Arith.nat -> Arith.nat list
end = struct

let rec from_to x y = List.upt x (Arith.suc y);;

end;;

module Monad : sig
  type ('a, 'b) m_t = M of ('b -> 'b * ('a, Util.error_t) Util.rresult)
  val dest_M : ('a, 'b) m_t -> 'b -> 'b * ('a, Util.error_t) Util.rresult
  val bind : ('a -> ('b, 'c) m_t) -> ('a, 'c) m_t -> ('b, 'c) m_t
  val fmap : ('a -> 'b) -> ('a, 'c) m_t -> ('b, 'c) m_t
  val return : 'a -> ('a, 'b) m_t
end = struct

type ('a, 'b) m_t = M of ('b -> 'b * ('a, Util.error_t) Util.rresult);;

let rec dest_M m = let M x = m in
                   x;;

let rec bind
  f m = M (fun s ->
            (match dest_M m s with (s1, Util.Ok y) -> dest_M (f y) s1
              | (s1, Util.Error x) -> (s1, Util.Error x)));;

let rec fmap
  f m = M (fun s ->
            let (sa, r) = dest_M m s in
            (sa, (match r with Util.Ok y -> Util.Ok (f y)
                   | Util.Error a -> Util.Error a)));;

let rec return x = M (fun s -> (s, Util.Ok x));;

end;;



module type Constants_t = sig
  type min_size_t = Small_root_node_or_leaf | Small_node | Small_leaf
  val max_leaf_size : Arith.nat
  val max_node_keys : Arith.nat
  val min_leaf_size : Arith.nat
  val min_node_keys : Arith.nat
end (*= struct

type min_size_t = Small_root_node_or_leaf | Small_node | Small_leaf;;

let max_leaf_size : Arith.nat = Util.failwitha "FIXME";;

let max_node_keys : Arith.nat = Util.failwitha "FIXME";;

let min_leaf_size : Arith.nat = Util.failwitha "FIXME";;

let min_node_keys : Arith.nat = Util.failwitha "FIXME";;


end;; *)

module type Key_value_types_t = sig
  type key [@@deriving yojson]
  val equal_keya : key -> key -> bool
  val equal_key : key HOL.equal
  type value_t [@@deriving yojson]
  val equal_value_t : value_t HOL.equal
  val key_ord : key -> key -> Arith.int
end (*= struct

type key = Private_key of Arith.nat;;

let rec equal_keya (Private_key x) (Private_key ya) = Arith.equal_nat x ya;;

let equal_key = ({HOL.equal = equal_keya} : key HOL.equal);;

type value_t = Private_value of Arith.nat;;

let rec equal_value_ta
  (Private_value x) (Private_value ya) = Arith.equal_nat x ya;;

let equal_value_t = ({HOL.equal = equal_value_ta} : value_t HOL.equal);;

let rec key_ord k1 k2 = Util.failwitha "key_ord";;

end;; *)

module type Store_t = sig
  type page
  type store
  type page_ref [@@deriving yojson]
  val free : page_ref list -> (unit, store) Monad.m_t
  val alloc : page -> (page_ref, store) Monad.m_t
  val dest_Store : store -> page_ref -> page
  val page_ref_to_page : page_ref -> (page, store) Monad.m_t
end (*= struct

type page = Page of Arith.nat;;

type store = Store of Arith.nat;;

type page_ref = Page_ref of Arith.nat;;

let rec free ps = Util.failwitha "FIXME";;

let rec alloc p = Util.failwitha "FIXME";;

let rec dest_Store s r = Util.failwitha "FIXME";;

let rec page_ref_to_page p = Util.failwitha "FIXME";;

end;; *)

module type Frame_types_t = sig
  module Store : Store_t
  module Key_value_types : Key_value_types_t
  type pframe = Node_frame of (Key_value_types.key list * Store.page_ref list) |
    Leaf_frame of (Key_value_types.key * Key_value_types.value_t) list [@@deriving yojson]
  val frame_to_page : pframe -> Store.page
  val page_to_frame : Store.page -> pframe
end (* = struct

type pframe = Node_frame of (Key_value_types.key list * Store.page_ref list) |
  Leaf_frame of (Key_value_types.key * Key_value_types.value_t) list;;

let rec frame_to_page x = Util.failwitha "FIXME" x;;

let rec page_to_frame x = Util.failwitha "FIXME" x;;

end;; *)

module Make = functor (Constants : Constants_t) -> functor (FT:Frame_types_t) -> struct

module Frame_types = FT
module Key_value_types = Frame_types.Key_value_types
module Store = Frame_types.Store

module Key_value : sig
  val key_eq : Key_value_types.key -> Key_value_types.key -> bool
  val key_lt : Key_value_types.key -> Key_value_types.key -> bool
  val check_keys :
    Key_value_types.key option ->
      Key_value_types.key Set.set -> Key_value_types.key option -> bool
  val kvs_delete :
    Key_value_types.key ->
      (Key_value_types.key * Key_value_types.value_t) list ->
        (Key_value_types.key * Key_value_types.value_t) list
  val kvs_insert :
    Key_value_types.key * Key_value_types.value_t ->
      (Key_value_types.key * Key_value_types.value_t) list ->
        (Key_value_types.key * Key_value_types.value_t) list
  val split_leaf :
    (Key_value_types.key * Key_value_types.value_t) list ->
      (Key_value_types.key * Key_value_types.value_t) list *
        (Key_value_types.key *
          (Key_value_types.key * Key_value_types.value_t) list)
  val split_node :
    Key_value_types.key list * 'a list ->
      (Key_value_types.key list * 'a list) *
        (Key_value_types.key * (Key_value_types.key list * 'a list))
  val split_ks_rs :
    Key_value_types.key ->
      Key_value_types.key list * 'a list ->
        (Key_value_types.key list * 'a list) *
          ('a * (Key_value_types.key list * 'a list))
  val ordered_key_list : Key_value_types.key list -> bool
end = struct

let rec key_eq
  k1 k2 = Arith.equal_int (Key_value_types.key_ord k1 k2) Arith.zero_int;;

let rec key_lt
  k1 k2 = Arith.less_int (Key_value_types.key_ord k1 k2) Arith.zero_int;;

let rec key_le k1 k2 = key_lt k1 k2 || key_eq k1 k2;;

let rec check_keys
  kl ks kr =
    let b1 = (match kl with None -> true | Some kla -> Set.ball ks (key_le kla))
      in
    let a =
      (match kr with None -> true
        | Some kra -> Set.ball ks (fun k -> key_lt k kra))
      in
    b1 && a;;

let rec kvs_delete
  k kvs =
    List.filter
      (fun kv -> not (Key_value_types.equal_keya (Product_Type.fst kv) k)) kvs;;

let rec kvs_insert
  kv x1 = match kv, x1 with kv, [] -> [kv]
    | kva, kv :: kvs ->
        let (k, v) = kva in
        let (ka, va) = kv in
        let i = Key_value_types.key_ord ka k in
        (if Arith.less_int i Arith.zero_int then (ka, va) :: kvs_insert kva kvs
          else (if Arith.equal_int i Arith.zero_int then (k, v) :: kvs
                 else (k, v) :: (ka, va) :: kvs));;

let rec split_leaf
  kvs = let cut_point =
          Arith.minus_nat (Arith.plus_nat Constants.max_leaf_size Arith.one_nat)
            Constants.min_leaf_size
          in
        let (l, r) = Util.split_at cut_point kvs in
        let _ =
          Util.assert_truea
            (Arith.less_eq_nat Constants.min_leaf_size (List.size_list l) &&
              Arith.less_eq_nat Constants.min_leaf_size (List.size_list r))
          in
        let k =
          (match r with [] -> Util.impossible1 "key_value, split_leaf"
            | (k, _) :: _ -> k)
          in
        (l, (k, r));;

let rec split_node
  n = let (ks, rs) = n in
      let cut_point =
        Arith.minus_nat Constants.max_node_keys Constants.min_node_keys in
      let (ks1, (k, ks2)) = Util.split_at_3 cut_point ks in
      let _ =
        Util.assert_truea
          (Arith.less_eq_nat Constants.min_node_keys (List.size_list ks2))
        in
      let (rs1, rs2) = Util.split_at (Arith.plus_nat cut_point Arith.one_nat) rs
        in
      ((ks1, rs1), (k, (ks2, rs2)));;

let rec search_key_to_index
  ks k =
    let num_keys = List.size_list ks in
    let i =
      List.find (fun x -> key_lt k (List.nth ks x))
        (List.upt Arith.zero_nat num_keys)
      in
    let ia = (match i with None -> num_keys | Some x -> x) in
    ia;;

let rec split_ks_rs
  k ks_rs =
    let (ks, rs) = ks_rs in
    let i = search_key_to_index ks k in
    let (ks1, ks2) = Util.split_at i ks in
    let (rs1, (r, rs2)) = Util.split_at_3 i rs in
    ((ks1, rs1), (r, (ks2, rs2)));;

let rec ordered_key_list
  ks = Arith.less_nat (List.size_list ks)
         (Arith.nat_of_integer (Big_int.big_int_of_int 2)) ||
         List.pred_list
           (fun i ->
             key_lt (List.nth ks i)
               (List.nth ks (Arith.plus_nat i Arith.one_nat)))
           (Prelude.from_to Arith.zero_nat
             (Arith.minus_nat (List.size_list ks)
               (Arith.nat_of_integer (Big_int.big_int_of_int 2))));;


end;;


module Tree : sig
  type tree = Node of (Key_value_types.key list * tree list) |
    Leaf of (Key_value_types.key * Key_value_types.value_t) list[@@deriving yojson]
  val equal_tree : tree HOL.equal
  val equal_treea : tree -> tree -> bool
  val dest_Node : tree -> Key_value_types.key list * tree list
  val tree_to_leaves :
    tree -> ((Key_value_types.key * Key_value_types.value_t) list) list
  val tree_to_kvs : tree -> (Key_value_types.key * Key_value_types.value_t) list
  val tree_to_keys : tree -> Key_value_types.key Set.set
  val wellformed_tree : Constants.min_size_t option -> tree -> bool
end = struct

type tree = Node of (Key_value_types.key list * tree list) |
  Leaf of (Key_value_types.key * Key_value_types.value_t) list[@@deriving yojson];;

let rec equal_tree () = ({HOL.equal = equal_treea} : tree HOL.equal)
and equal_treea
  x0 x1 = match x0, x1 with Node x1, Leaf x2 -> false
    | Leaf x2, Node x1 -> false
    | Leaf x2, Leaf y2 ->
        List.equal_lista
          (Product_Type.equal_prod Key_value_types.equal_key
            Key_value_types.equal_value_t)
          x2 y2
    | Node x1, Node y1 ->
        Product_Type.equal_proda (List.equal_list Key_value_types.equal_key)
          (List.equal_list (equal_tree ())) x1 y1;;
let equal_tree = equal_tree ();;

let rec tree_to_subtrees
  t0 = (match t0
         with Node (_, cs) ->
           t0 :: Util.rev_apply (List.map tree_to_subtrees cs) List.concat
         | Leaf _ -> [t0]);;

let rec keys_1
  t0 = (match t0 with Node (l, _) -> l
         | Leaf a -> List.map Product_Type.fst a);;

let rec keys
  t0 = Util.rev_apply
         (Util.rev_apply (Util.rev_apply t0 tree_to_subtrees) (List.map keys_1))
         List.concat;;

let rec height
  t0 = (match t0
         with Node (_, cs) ->
           Arith.plus_nat Arith.one_nat
             (Lattices_Big.max Arith.linorder_nat
               (Set.Set (List.map height cs)))
         | Leaf _ -> Arith.one_nat);;

let rec forall_subtrees
  p t = List.pred_list p (Util.rev_apply t tree_to_subtrees);;

let rec get_min_size
  mt = (match mt
         with (Constants.Small_root_node_or_leaf, Node _) -> Arith.one_nat
         | (Constants.Small_root_node_or_leaf, Leaf _) -> Arith.zero_nat
         | (Constants.Small_node, Node _) ->
           Arith.minus_nat Constants.min_node_keys Arith.one_nat
         | (Constants.Small_leaf, Leaf _) ->
           Arith.minus_nat Constants.min_leaf_size Arith.one_nat);;

let rec wf_size_1
  t1 = (match t1
         with Node (l, _) ->
           let n = List.size_list l in
           Arith.less_eq_nat Arith.one_nat n &&
             (Arith.less_eq_nat Constants.min_node_keys n &&
               Arith.less_eq_nat n Constants.max_node_keys)
         | Leaf xs ->
           let n = List.size_list xs in
           Arith.less_eq_nat Constants.min_leaf_size n &&
             Arith.less_eq_nat n Constants.max_leaf_size);;

let rec wf_size
  ms t0 =
    Util.assert_true (ms, t0)
      (match ms with None -> forall_subtrees wf_size_1 t0
        | Some m ->
          let min = get_min_size (m, t0) in
          (match t0
            with Node (l, cs) ->
              let n = List.size_list l in
              Arith.less_eq_nat min n &&
                (Arith.less_eq_nat n Constants.max_node_keys &&
                  List.pred_list (forall_subtrees wf_size_1) cs)
            | Leaf xs ->
              let n = List.size_list xs in
              Arith.less_eq_nat min n &&
                Arith.less_eq_nat n Constants.max_leaf_size));;

let rec balanced_1
  t0 = (match t0
         with Node (_, cs) ->
           List.null cs ||
             List.pred_list
               (fun c ->
                 Arith.equal_nat (height c)
                   (height (List.nth cs Arith.zero_nat)))
               cs
         | Leaf _ -> true);;

let rec balanced t = Util.assert_true t (forall_subtrees balanced_1 t);;

let rec wf_ks_rs_1
  t0 = (match t0
         with Node (l, cs) ->
           Arith.equal_nat (Arith.plus_nat Arith.one_nat (List.size_list l))
             (List.size_list cs)
         | Leaf _ -> true);;

let rec wf_ks_rs t0 = Util.assert_true t0 (forall_subtrees wf_ks_rs_1 t0);;

let rec dest_Node
  = function Node (ks, rs) -> (ks, rs)
    | Leaf uu -> Util.failwitha "dest_Node";;

let rec tree_to_leaves
  t0 = (match t0
         with Node (_, cs) ->
           Util.rev_apply (List.map tree_to_leaves cs) List.concat
         | Leaf l -> [l]);;

let rec tree_to_kvs
  t = Util.rev_apply (Util.rev_apply t tree_to_leaves) List.concat;;

let rec keys_ordered_1
  t0 = Util.rev_apply (Util.rev_apply t0 keys_1) Key_value.ordered_key_list;;

let rec keys_ordered t = Util.assert_true t (forall_subtrees keys_ordered_1 t);;

let rec tree_to_keys
  t = Util.rev_apply
        (Util.rev_apply (Util.rev_apply t tree_to_kvs)
          (List.map Product_Type.fst))
        (fun a -> Set.Set a);;

let rec ks_to_max_child_index ks = List.size_list ks;;

let min_child_index : Arith.nat = Arith.zero_nat;;

let rec index_to_bound
  ks i =
    let l =
      (if Arith.equal_nat i min_child_index then None
        else Some (List.nth ks (Arith.minus_nat i Arith.one_nat)))
      in
    let a =
      (if Arith.less_eq_nat (ks_to_max_child_index ks) i then None
        else Some (List.nth ks i))
      in
    (l, a);;

let rec subtree_indexes
  node =
    let (ks, _) = node in
    Prelude.from_to min_child_index (ks_to_max_child_index ks);;

let rec keys_consistent_1
  t0 = (match t0
         with Node (ks, rs) ->
           List.pred_list
             (fun i ->
               let a = index_to_bound ks i in
               let (l, aa) = a in
               Key_value.check_keys l (Set.Set (keys (List.nth rs i))) aa)
             (subtree_indexes (ks, rs))
         | Leaf _ -> true);;

let rec keys_consistent
  t = Util.assert_true t (forall_subtrees keys_consistent_1 t);;

let rec wellformed_tree
  ms t0 =
    Util.assert_true (ms, t0)
      (let b1 = wf_size ms t0 in
       let b2 = wf_ks_rs t0 in
       let b3 = balanced t0 in
       let b4 = keys_consistent t0 in
       let b5 = keys_ordered t0 in
       let wf = b1 && (b2 && (b3 && (b4 && b5))) in
       wf);;

end;;

module Tree_stack : sig
  type ('a, 'b) frame_ext =
    Frame_ext of
      Key_value_types.key list * 'a list * 'a * Key_value_types.key list *
        'a list * 'b [@@deriving yojson]
  val equal_frame_ext :
    'a HOL.equal -> 'b HOL.equal -> ('a, 'b) frame_ext HOL.equal
  val no_focus :
    (Tree.tree, unit) frame_ext list -> (Tree.tree, unit) frame_ext list
  val f_t : ('a, 'b) frame_ext -> 'a
  val frame_map : ('a -> 'b) -> ('a, unit) frame_ext -> ('b, unit) frame_ext
  val stack_map :
    ('a -> 'b) -> ('a, unit) frame_ext list -> ('b, unit) frame_ext list
  val dest_frame :
    ('a, unit) frame_ext ->
      (Key_value_types.key list * 'a list) *
        ('a * (Key_value_types.key list * 'a list))
  val tree_to_stack :
    Key_value_types.key ->
      Tree.tree -> Arith.nat -> Tree.tree * (Tree.tree, unit) frame_ext list
  val stack_to_lu_of_child :
    ('a, unit) frame_ext list ->
      Key_value_types.key option * Key_value_types.key option
  val add_new_stk_frame :
    Key_value_types.key ->
      Key_value_types.key list * 'a list ->
        ('a, unit) frame_ext list -> ('a, unit) frame_ext list * 'a
end = struct

type ('a, 'b) frame_ext =
  Frame_ext of
    Key_value_types.key list * 'a list * 'a * Key_value_types.key list *
      'a list * 'b [@@deriving yojson];;

let rec equal_frame_exta _A _B
  (Frame_ext (f_ks1a, f_ts1a, f_ta, f_ks2a, f_ts2a, morea))
    (Frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) =
    List.equal_lista Key_value_types.equal_key f_ks1a f_ks1 &&
      (List.equal_lista _A f_ts1a f_ts1 &&
        (HOL.eq _A f_ta f_t &&
          (List.equal_lista Key_value_types.equal_key f_ks2a f_ks2 &&
            (List.equal_lista _A f_ts2a f_ts2 && HOL.eq _B morea more))));;

let rec equal_frame_ext _A _B =
  ({HOL.equal = equal_frame_exta _A _B} : ('a, 'b) frame_ext HOL.equal);;

let rec f_t_update
  f_ta (Frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) =
    Frame_ext (f_ks1, f_ts1, f_ta f_t, f_ks2, f_ts2, more);;

let rec no_focus
  stk = (match stk with [] -> []
          | frm :: a -> f_t_update (fun _ -> Tree.Leaf []) frm :: a);;

let rec f_t (Frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_t;;

let rec f_ts2 (Frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ts2;;

let rec f_ts1 (Frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ts1;;

let rec f_ks2 (Frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ks2;;

let rec f_ks1 (Frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ks1;;

let rec frame_map
  g f = Frame_ext
          (Util.rev_apply f f_ks1,
            Util.rev_apply (Util.rev_apply f f_ts1) (List.map g),
            Util.rev_apply (Util.rev_apply f f_t) g, Util.rev_apply f f_ks2,
            Util.rev_apply (Util.rev_apply f f_ts2) (List.map g), ());;

let rec stack_map f stk = Util.rev_apply stk (List.map (frame_map f));;

let rec dest_frame
  f = ((Util.rev_apply f f_ks1, Util.rev_apply f f_ts1),
        (Util.rev_apply f f_t,
          (Util.rev_apply f f_ks2, Util.rev_apply f f_ts2)));;

let rec tree_to_stack
  k t n =
    (if Arith.equal_nat n Arith.zero_nat then (t, [])
      else (match tree_to_stack k t (Arith.minus_nat n Arith.one_nat)
             with (Tree.Node (ks, ts), stk) ->
               let a = Key_value.split_ks_rs k (ks, ts) in
               let (aa, b) = a in
               let (ks1, ts1) = aa in
               (fun (ta, (ks2, ts2)) ->
                 let frm = Frame_ext (ks1, ts1, ta, ks2, ts2, ()) in
                 (ta, frm :: stk))
                 b
             | (Tree.Leaf _, _) -> Util.failwitha "tree_to_stack"));;

let rec stack_to_lu_of_child
  = function [] -> (None, None)
    | x :: stk ->
        let (l, u) = stack_to_lu_of_child stk in
        let (ks1, ks2) = (Util.rev_apply x f_ks1, Util.rev_apply x f_ks2) in
        let la =
          (if not (List.null ks1) then Some (Util.rev_apply ks1 List.last)
            else l)
          in
        let a =
          (if not (List.null ks2) then Some (Util.rev_apply ks2 List.hd) else u)
          in
        (la, a);;

let rec add_new_stk_frame
  k ks_rs stk =
    let (ks, rs) = ks_rs in
    let a = Key_value.split_ks_rs k (ks, rs) in
    let (aa, b) = a in
    let (ks1, rs1) = aa in
    (fun (r, (ks2, rs2)) ->
      let (_, _) = stack_to_lu_of_child stk in
      let frm = Frame_ext (ks1, rs1, r, ks2, rs2, ()) in
      (frm :: stk, r))
      b;;

end;;

module Frame : sig
  val r_to_t : Store.store -> Store.page_ref -> Tree.tree
  val r_stk_to_rs :
    (Store.page_ref, unit) Tree_stack.frame_ext list -> Store.page_ref list
  val dest_Leaf_frame :
    Frame_types.pframe -> (Key_value_types.key * Key_value_types.value_t) list
  val dest_Node_frame :
    Frame_types.pframe -> Key_value_types.key list * Store.page_ref list
  val page_ref_to_frame :
    Store.page_ref -> (Frame_types.pframe, Store.store) Monad.m_t
  val r_frame_to_t_frame :
    Store.store ->
      (Store.page_ref, unit) Tree_stack.frame_ext ->
        (Tree.tree, unit) Tree_stack.frame_ext
end = struct

let rec r_to_ta
  s n r =
    let r_to_p = Store.dest_Store s in
    (if Arith.equal_nat n Arith.zero_nat then Util.failwitha "r_to_t"
      else Util.rev_apply (r_to_p r)
             (fun page ->
               Util.rev_apply (Util.rev_apply page Frame_types.page_to_frame)
                 (fun a ->
                   (match a
                     with Frame_types.Node_frame (ks, rs) ->
                       Tree.Node
                         (ks, List.map
                                (r_to_ta s (Arith.minus_nat n Arith.one_nat))
                                rs)
                     | Frame_types.Leaf_frame aa -> Tree.Leaf aa))));;

let rec r_to_t
  s r = r_to_ta s (Arith.nat_of_integer (Big_int.big_int_of_int 1000)) r;;

let rec r_stk_to_rs xs = Util.rev_apply xs (List.map Tree_stack.f_t);;

let rec dest_Leaf_frame
  f = (match f with Frame_types.Node_frame _ -> Util.failwitha "dest_Leaf_frame"
        | Frame_types.Leaf_frame x -> x);;

let rec dest_Node_frame
  f = (match f with Frame_types.Node_frame x -> x
        | Frame_types.Leaf_frame _ -> Util.failwitha "dest_Node_frame");;

let rec page_ref_to_frame
  r = Util.rev_apply (Store.page_ref_to_page r)
        (Monad.fmap Frame_types.page_to_frame);;

let rec r_frame_to_t_frame s = Tree_stack.frame_map (r_to_t s);;

end;;

module Find : sig
  type find_state = 
  F_down of
    (Store.page_ref *
      (Key_value_types.key *
        (Store.page_ref * (Store.page_ref, unit) Tree_stack.frame_ext list)))
  | F_finished of
      (Store.page_ref *
        (Key_value_types.key *
          (Store.page_ref *
            ((Key_value_types.key * Key_value_types.value_t) list *
              (Store.page_ref, unit) Tree_stack.frame_ext list)))) [@@deriving yojson];;
  val find_step : find_state -> (find_state, Store.store) Monad.m_t
  val empty_btree : unit -> (Store.page_ref, Store.store) Monad.m_t
  val mk_find_state : Key_value_types.key -> Store.page_ref -> find_state
  val dest_f_finished :
    find_state ->
      (Store.page_ref *
        (Key_value_types.key *
          (Store.page_ref *
            ((Key_value_types.key * Key_value_types.value_t) list *
              (Store.page_ref, unit) Tree_stack.frame_ext list)))) option
  val wellformed_find_state : Store.store -> Tree.tree -> find_state -> bool
end = struct

type find_state =
  F_down of
    (Store.page_ref *
      (Key_value_types.key *
        (Store.page_ref * (Store.page_ref, unit) Tree_stack.frame_ext list)))
  | F_finished of
      (Store.page_ref *
        (Key_value_types.key *
          (Store.page_ref *
            ((Key_value_types.key * Key_value_types.value_t) list *
              (Store.page_ref, unit) Tree_stack.frame_ext list)))) [@@deriving yojson];;

let rec find_step
  fs = (match fs
         with F_down (r0, (k, (r, stk))) ->
           Util.rev_apply (Frame.page_ref_to_frame r)
             (Monad.fmap
               (fun a ->
                 (match a
                   with Frame_types.Node_frame (ks, rs) ->
                     let (stka, ra) =
                       Tree_stack.add_new_stk_frame k (ks, rs) stk in
                     F_down (r0, (k, (ra, stka)))
                   | Frame_types.Leaf_frame kvs ->
                     F_finished (r0, (k, (r, (kvs, stk)))))))
         | F_finished _ -> Monad.return fs);;

let rec empty_btree
  uu = let lf = Frame_types.Leaf_frame [] in
       Util.rev_apply (Util.rev_apply lf Frame_types.frame_to_page)
         Store.alloc;;

let rec mk_find_state k r = F_down (r, (k, (r, [])));;

let rec dest_f_finished
  fs = (match fs with F_down _ -> None
         | F_finished (r0, (k, (r, (kvs, stk)))) ->
           Some (r0, (k, (r, (kvs, stk)))));;

let rec wellformed_find_state
  s t0 fs =
    Util.assert_truea
      (let r_to_t = Frame.r_to_t s in
       (match fs
         with F_down (_, (k, (r, stk))) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k t0 (List.size_list stk) in
           Product_Type.equal_proda Tree.equal_tree
             (List.equal_list
               (Tree_stack.equal_frame_ext Tree.equal_tree
                 Product_Type.equal_unit))
             (t_fo, t_stk)
             (r_to_t r, Util.rev_apply stk (Tree_stack.stack_map r_to_t))
         | F_finished (_, (k, (_, (kvs, stk)))) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k t0 (List.size_list stk) in
           Product_Type.equal_proda Tree.equal_tree
             (List.equal_list
               (Tree_stack.equal_frame_ext Tree.equal_tree
                 Product_Type.equal_unit))
             (t_fo, t_stk)
             (Tree.Leaf kvs,
               Util.rev_apply stk (Tree_stack.stack_map r_to_t))));;

end;;

module Delete : sig
  type del_t =
    D_small_leaf of (Key_value_types.key * Key_value_types.value_t) list |
    D_small_node of (Key_value_types.key list * Store.page_ref list) |
    D_updated_subtree of Store.page_ref [@@deriving yojson]
  type d_state = D_down of (Find.find_state * Store.page_ref) |
    D_up of
      (del_t *
        ((Store.page_ref, unit) Tree_stack.frame_ext list * Store.page_ref))
    | D_finished of Store.page_ref
  val delete_step : d_state -> (d_state, Store.store) Monad.m_t
  val dest_d_finished : d_state -> Store.page_ref option
  val mk_delete_state : Key_value_types.key -> Store.page_ref -> d_state
  val wellformed_delete_state :
    Tree.tree -> Key_value_types.key -> Store.store -> d_state -> bool
end = struct

type 'a d12_t = D1 of 'a | D2 of ('a * (Key_value_types.key * 'a));;

type del_t =
  D_small_leaf of (Key_value_types.key * Key_value_types.value_t) list |
  D_small_node of (Key_value_types.key list * Store.page_ref list) |
  D_updated_subtree of Store.page_ref [@@deriving yojson];;

type d_state = D_down of (Find.find_state * Store.page_ref) |
  D_up of
    (del_t *
      ((Store.page_ref, unit) Tree_stack.frame_ext list * Store.page_ref))
  | D_finished of Store.page_ref [@@deriving yojson];;

let rec wf_d
  t0 s d =
    Util.assert_truea (let (fs, _) = d in
                       Find.wellformed_find_state s t0 fs);;

let rec wf_f
  t0 k s r =
    Util.assert_truea
      (let t = Frame.r_to_t s r in
       Tree.wellformed_tree (Some Constants.Small_root_node_or_leaf) t &&
         List.equal_lista
           (Product_Type.equal_prod Key_value_types.equal_key
             Key_value_types.equal_value_t)
           (Util.rev_apply (Util.rev_apply t0 Tree.tree_to_kvs)
             (Key_value.kvs_delete k))
           (Util.rev_apply t Tree.tree_to_kvs));;

let rec wf_u
  t0 k s u =
    Util.assert_truea
      (let r_to_t = Frame.r_to_t s in
       (match u
         with (D_small_leaf kvs, stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Constants.Small_root_node_or_leaf
               | _ :: _ -> Some Constants.Small_leaf)
             in
           List.equal_lista
             (Tree_stack.equal_frame_ext Tree.equal_tree
               Product_Type.equal_unit)
             (Util.rev_apply t_stk Tree_stack.no_focus)
             (Util.rev_apply (Util.rev_apply stk (Tree_stack.stack_map r_to_t))
               Tree_stack.no_focus) &&
             (Tree.wellformed_tree ms (Tree.Leaf kvs) &&
               List.equal_lista
                 (Product_Type.equal_prod Key_value_types.equal_key
                   Key_value_types.equal_value_t)
                 (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                   (Key_value.kvs_delete k))
                 kvs)
         | (D_small_node (ks, rs), stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Constants.Small_root_node_or_leaf
               | _ :: _ -> Some Constants.Small_node)
             in
           let t = Tree.Node (ks, Util.rev_apply rs (List.map r_to_t)) in
           List.equal_lista
             (Tree_stack.equal_frame_ext Tree.equal_tree
               Product_Type.equal_unit)
             (Util.rev_apply t_stk Tree_stack.no_focus)
             (Util.rev_apply (Util.rev_apply stk (Tree_stack.stack_map r_to_t))
               Tree_stack.no_focus) &&
             (Tree.wellformed_tree ms t &&
               List.equal_lista
                 (Product_Type.equal_prod Key_value_types.equal_key
                   Key_value_types.equal_value_t)
                 (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                   (Key_value.kvs_delete k))
                 (Util.rev_apply t Tree.tree_to_kvs))
         | (D_updated_subtree r, stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Constants.Small_root_node_or_leaf
               | _ :: _ -> None)
             in
           let t = Util.rev_apply r r_to_t in
           List.equal_lista
             (Tree_stack.equal_frame_ext Tree.equal_tree
               Product_Type.equal_unit)
             (Util.rev_apply t_stk Tree_stack.no_focus)
             (Util.rev_apply (Util.rev_apply stk (Tree_stack.stack_map r_to_t))
               Tree_stack.no_focus) &&
             (Tree.wellformed_tree ms t &&
               List.equal_lista
                 (Product_Type.equal_prod Key_value_types.equal_key
                   Key_value_types.equal_value_t)
                 (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                   (Key_value.kvs_delete k))
                 (Util.rev_apply t Tree.tree_to_kvs))));;

let rec frac_mult
  xs ys =
    let a = xs in
    let (aa, b) = a in
    let (aaa, ba) = ys in
    (aa @ aaa, b @ ba);;

let rec post_steal_or_merge
  stk p p_1 p_2 x =
    let m = frac_mult in
    (match x
      with D1 c ->
        let pa = Frame_types.Node_frame (m (m p_1 ([], [c])) p_2) in
        let p_sz =
          Util.rev_apply
            (Util.rev_apply (Util.rev_apply pa Frame.dest_Node_frame)
              Product_Type.fst)
            List.size_list
          in
        let f =
          (match Arith.equal_nat p_sz Arith.zero_nat
            with true ->
              let _ = Util.assert_true (List.null stk) in
              Monad.return (D_updated_subtree c)
            | false ->
              (match Arith.less_nat p_sz Constants.min_node_keys
                with true ->
                  Monad.return
                    (D_small_node (Util.rev_apply pa Frame.dest_Node_frame))
                | false ->
                  Util.rev_apply
                    (Util.rev_apply
                      (Util.rev_apply pa Frame_types.frame_to_page) Store.alloc)
                    (Monad.fmap (fun a -> D_updated_subtree a))))
          in
        Util.rev_apply f (Monad.fmap (fun fa -> (fa, stk)))
      | D2 (c1, (k, c2)) ->
        let pa = Frame_types.Node_frame (m (m p_1 ([k], [c1; c2])) p_2) in
        let p_sz =
          Util.rev_apply
            (Util.rev_apply (Util.rev_apply pa Frame.dest_Node_frame)
              Product_Type.fst)
            List.size_list
          in
        let f =
          (match Arith.less_nat p_sz Constants.min_node_keys
            with true ->
              let _ = Util.assert_true (List.null stk) in
              Monad.return
                (D_small_node (Util.rev_apply pa Frame.dest_Node_frame))
            | false ->
              Util.rev_apply
                (Util.rev_apply (Util.rev_apply pa Frame_types.frame_to_page)
                  Store.alloc)
                (Monad.fmap (fun a -> D_updated_subtree a)))
          in
        Util.rev_apply f (Monad.fmap (fun fa -> (fa, stk))));;

let rec steal_or_merge
  right leaf mk_c c p_k s =
    let m = frac_mult in
    let (s_ks, s_ts) = s in
    let a =
      (match right
        with true ->
          let a = (Util.dest_list s_ks, Util.dest_list s_ts) in
          let (aa, b) = a in
          let (k, ks) = aa in
          (fun (t, ts) -> ((k, t), (ks, ts)))
            b
        | false ->
          let a = (Util.dest_lista s_ks, Util.dest_lista s_ts) in
          let (aa, b) = a in
          let (ks, k) = aa in
          (fun (ts, t) -> ((k, t), (ks, ts)))
            b)
      in
    let (aa, b) = a in
    let (s_k, s_t) = aa in
    (fun s_1 ->
      (match
        Arith.less_nat
          (if leaf then Constants.min_leaf_size else Constants.min_node_keys)
          (Arith.plus_nat Arith.one_nat (List.size_list (Product_Type.fst s_1)))
        with true ->
          let ca =
            let k = (if leaf then s_k else p_k) in
            (if right then m c ([k], [s_t]) else m ([k], [s_t]) c)
            in
          let sa = mk_c s_1 in
          let p_ka =
            (if leaf
              then let right_sib = (if right then s_1 else ca) in
                   Util.rev_apply (Util.rev_apply right_sib Product_Type.fst)
                     List.hd
              else s_k)
            in
          let cb = mk_c ca in
          (if right then D2 (cb, (p_ka, sa)) else D2 (sa, (p_ka, cb)))
        | false ->
          let k = (if leaf then ([], []) else ([p_k], [])) in
          let ab = mk_c (if right then m (m c k) s else m s (m k c)) in
          D1 ab))
      b;;

let rec get_sibling
  p = let (p_1, p_2) = p in
      (match p_2
        with ([], _) ->
          (match p_1 with ([], _) -> Util.impossible1 "delete, get_sibling"
            | (_ :: _, []) -> Util.impossible1 "delete, get_sibling"
            | (_ :: _, _ :: _) ->
              let right = false in
              let (p_ks1, p_ts1) = p_1 in
              let (p_1_ks, p_k) = Util.dest_lista p_ks1 in
              let (p_1_ts, s) = Util.dest_lista p_ts1 in
              let (p_1a, _) = ((p_1_ks, p_1_ts), p_2) in
              (right, ((p_1a, p_2), (p_k, s))))
        | (_ :: _, []) ->
          (match p_1 with ([], _) -> Util.impossible1 "delete, get_sibling"
            | (_ :: _, []) -> Util.impossible1 "delete, get_sibling"
            | (_ :: _, _ :: _) ->
              let right = false in
              let (p_ks1, p_ts1) = p_1 in
              let (p_1_ks, p_k) = Util.dest_lista p_ks1 in
              let (p_1_ts, s) = Util.dest_lista p_ts1 in
              let (p_1a, _) = ((p_1_ks, p_1_ts), p_2) in
              (right, ((p_1a, p_2), (p_k, s))))
        | (p_k :: p_ks2, r :: p_ts2) ->
          let right = true in
          (right, ((p_1, (p_ks2, p_ts2)), (p_k, r))));;

let rec step_up
  du = (match du with (_, []) -> Util.impossible1 "delete, step_up"
         | (D_small_leaf kvs, p :: stk) ->
           let leaf = true in
           let mk_c = (fun (ks, vs) -> Frame_types.Leaf_frame (List.zip ks vs))
             in
           let a = Util.rev_apply p Tree_stack.dest_frame in
           let (aa, b) = a in
           let (p_ks1, p_rs1) = aa in
           (fun (_, (p_ks2, p_rs2)) ->
             let ab = get_sibling ((p_ks1, p_rs1), (p_ks2, p_rs2)) in
             let (right, ac) = ab in
             let (ad, ba) = ac in
             let (p_1, p_2) = ad in
             (fun (p_k, r) ->
               let frm = Util.rev_apply r Frame.page_ref_to_frame in
               let d12 =
                 Util.rev_apply frm
                   (Monad.fmap
                     (fun frma ->
                       steal_or_merge right leaf mk_c
                         (Util.rev_apply kvs Util.unzip) p_k
                         (Util.rev_apply
                           (Util.rev_apply frma Frame.dest_Leaf_frame)
                           Util.unzip)))
                 in
               let d12a =
                 Util.rev_apply d12
                   (Monad.bind
                     (fun ae ->
                       (match ae
                         with D1 frma ->
                           Util.rev_apply
                             (Util.rev_apply
                               (Util.rev_apply frma Frame_types.frame_to_page)
                               Store.alloc)
                             (Monad.fmap (fun af -> D1 af))
                         | D2 (frm1, (p_ka, frm2)) ->
                           Util.rev_apply
                             (Util.rev_apply
                               (Util.rev_apply frm1 Frame_types.frame_to_page)
                               Store.alloc)
                             (Monad.bind
                               (fun r1 ->
                                 Util.rev_apply
                                   (Util.rev_apply
                                     (Util.rev_apply frm2
                                       Frame_types.frame_to_page)
                                     Store.alloc)
                                   (Monad.fmap
                                     (fun r2 -> D2 (r1, (p_ka, r2)))))))))
                 in
               Util.rev_apply d12a
                 (Monad.bind (post_steal_or_merge stk p p_1 p_2)))
               ba)
             b
         | (D_small_node (ks, rs), p :: stk) ->
           let leaf = false in
           let mk_c = (fun a -> Frame_types.Node_frame a) in
           let a = Util.rev_apply p Tree_stack.dest_frame in
           let (aa, b) = a in
           let (p_ks1, p_rs1) = aa in
           (fun (_, (p_ks2, p_rs2)) ->
             let ab = get_sibling ((p_ks1, p_rs1), (p_ks2, p_rs2)) in
             let (right, ac) = ab in
             let (ad, ba) = ac in
             let (p_1, p_2) = ad in
             (fun (p_k, r) ->
               let frm = Util.rev_apply r Frame.page_ref_to_frame in
               let d12 =
                 Util.rev_apply frm
                   (Monad.fmap
                     (fun frma ->
                       steal_or_merge right leaf mk_c (ks, rs) p_k
                         (Util.rev_apply frma Frame.dest_Node_frame)))
                 in
               let d12a =
                 Util.rev_apply d12
                   (Monad.bind
                     (fun ae ->
                       (match ae
                         with D1 frma ->
                           Util.rev_apply
                             (Util.rev_apply
                               (Util.rev_apply frma Frame_types.frame_to_page)
                               Store.alloc)
                             (Monad.fmap (fun af -> D1 af))
                         | D2 (frm1, (p_ka, frm2)) ->
                           Util.rev_apply
                             (Util.rev_apply
                               (Util.rev_apply frm1 Frame_types.frame_to_page)
                               Store.alloc)
                             (Monad.bind
                               (fun r1 ->
                                 Util.rev_apply
                                   (Util.rev_apply
                                     (Util.rev_apply frm2
                                       Frame_types.frame_to_page)
                                     Store.alloc)
                                   (Monad.fmap
                                     (fun r2 -> D2 (r1, (p_ka, r2)))))))))
                 in
               Util.rev_apply d12a
                 (Monad.bind (post_steal_or_merge stk p p_1 p_2)))
               ba)
             b
         | (D_updated_subtree r, p :: stk) ->
           let a = Util.rev_apply p Tree_stack.dest_frame in
           let (aa, b) = a in
           let (ks1, rs1) = aa in
           (fun (_, (ks2, rs2)) ->
             Util.rev_apply
               (Util.rev_apply
                 (Util.rev_apply
                   (Frame_types.Node_frame (ks1 @ ks2, rs1 @ [r] @ rs2))
                   Frame_types.frame_to_page)
                 Store.alloc)
               (Monad.fmap (fun ra -> (D_updated_subtree ra, stk))))
             b);;

let rec delete_step
  s = (match s
        with D_down (f, r0) ->
          (match Find.dest_f_finished f
            with None ->
              Util.rev_apply (Find.find_step f)
                (Monad.fmap (fun fa -> D_down (fa, r0)))
            | Some (r0a, (k, (_, (kvs, stk)))) ->
              Util.rev_apply (Store.free (r0a :: Frame.r_stk_to_rs stk))
                (Monad.bind
                  (fun _ ->
                    (match
                      List.member Key_value_types.equal_key
                        (Util.rev_apply kvs (List.map Product_Type.fst)) k
                      with true ->
                        let kvsa =
                          Util.rev_apply kvs
                            (List.filter
                              (fun x ->
                                not (Key_value.key_eq (Product_Type.fst x) k)))
                          in
                        (match
                          Arith.less_nat (List.size_list kvsa)
                            Constants.min_leaf_size
                          with true ->
                            Monad.return (D_up (D_small_leaf kvsa, (stk, r0a)))
                          | false ->
                            Util.rev_apply
                              (Util.rev_apply
                                (Util.rev_apply (Frame_types.Leaf_frame kvsa)
                                  Frame_types.frame_to_page)
                                Store.alloc)
                              (Monad.fmap
                                (fun r ->
                                  D_up (D_updated_subtree r, (stk, r0a)))))
                      | false -> Monad.return (D_finished r0a)))))
        | D_up (f, (stk, r0)) ->
          (match stk
            with [] ->
              (match f
                with D_small_leaf kvs ->
                  Util.rev_apply
                    (Util.rev_apply
                      (Util.rev_apply (Frame_types.Leaf_frame kvs)
                        Frame_types.frame_to_page)
                      Store.alloc)
                    (Monad.fmap (fun a -> D_finished a))
                | D_small_node (ks, rs) ->
                  Util.rev_apply
                    (Util.rev_apply
                      (Util.rev_apply (Frame_types.Node_frame (ks, rs))
                        Frame_types.frame_to_page)
                      Store.alloc)
                    (Monad.fmap (fun a -> D_finished a))
                | D_updated_subtree r -> Monad.return (D_finished r))
            | _ :: _ ->
              Util.rev_apply (step_up (f, stk))
                (Monad.fmap (fun (fa, stka) -> D_up (fa, (stka, r0)))))
        | D_finished _ -> Monad.return s);;

let rec dest_d_finished
  x = (match x with D_down _ -> None | D_up _ -> None
        | D_finished a -> Some a);;

let rec mk_delete_state k r = D_down (Find.mk_find_state k r, r);;

let rec wellformed_delete_state
  t0 k s ds =
    Util.assert_truea
      (match ds with D_down a -> wf_d t0 s a
        | D_up (fo, (stk, r)) ->
          wf_u t0 k s (fo, stk) && Tree.equal_treea (Frame.r_to_t s r) t0
        | D_finished a -> wf_f t0 k s a);;

end;;

module Insert : sig
  type i_t = I1 of Store.page_ref |
    I2 of (Store.page_ref * (Key_value_types.key * Store.page_ref)) [@@deriving yojson]
  type i_state_t = I_down of (Find.find_state * Key_value_types.value_t) |
    I_up of (i_t * (Store.page_ref, unit) Tree_stack.frame_ext list) |
    I_finished of Store.page_ref [@@deriving yojson]
  val insert_step : i_state_t -> (i_state_t, Store.store) Monad.m_t
  val dest_i_finished : i_state_t -> Store.page_ref option
  val mk_insert_state :
    Key_value_types.key ->
      Key_value_types.value_t -> Store.page_ref -> i_state_t
  val wellformed_insert_state :
    Tree.tree ->
      Key_value_types.key ->
        Key_value_types.value_t -> Store.store -> i_state_t -> bool
end = struct

type i_t = I1 of Store.page_ref |
  I2 of (Store.page_ref * (Key_value_types.key * Store.page_ref)) [@@deriving yojson];;

type i_state_t = I_down of (Find.find_state * Key_value_types.value_t) |
  I_up of (i_t * (Store.page_ref, unit) Tree_stack.frame_ext list) |
  I_finished of Store.page_ref [@@deriving yojson];;

let rec wf_d
  t0 s d =
    Util.assert_truea (let (fs, _) = d in
                       Find.wellformed_find_state s t0 fs);;

let rec wf_f
  t0 k v s r =
    Util.assert_truea
      (let t = Frame.r_to_t s r in
       Tree.wellformed_tree (Some Constants.Small_root_node_or_leaf) t &&
         List.equal_lista
           (Product_Type.equal_prod Key_value_types.equal_key
             Key_value_types.equal_value_t)
           (Util.rev_apply (Util.rev_apply t0 Tree.tree_to_kvs)
             (Key_value.kvs_insert (k, v)))
           (Util.rev_apply t Tree.tree_to_kvs));;

let rec wf_u
  t0 k v s u =
    Util.assert_truea
      (let r_to_t = Frame.r_to_t s in
       (match u
         with (I1 r, stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k t0 (List.size_list stk) in
           List.equal_lista
             (Tree_stack.equal_frame_ext Tree.equal_tree
               Product_Type.equal_unit)
             (Util.rev_apply t_stk Tree_stack.no_focus)
             (Util.rev_apply (Util.rev_apply stk (Tree_stack.stack_map r_to_t))
               Tree_stack.no_focus) &&
             List.equal_lista
               (Product_Type.equal_prod Key_value_types.equal_key
                 Key_value_types.equal_value_t)
               (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                 (Key_value.kvs_insert (k, v)))
               (Util.rev_apply (Util.rev_apply r r_to_t) Tree.tree_to_kvs)
         | (I2 (r1, (ka, r2)), stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k t0 (List.size_list stk) in
           List.equal_lista
             (Tree_stack.equal_frame_ext Tree.equal_tree
               Product_Type.equal_unit)
             (Util.rev_apply t_stk Tree_stack.no_focus)
             (Util.rev_apply (Util.rev_apply stk (Tree_stack.stack_map r_to_t))
               Tree_stack.no_focus) &&
             let (l, ua) = Tree_stack.stack_to_lu_of_child t_stk in
             let (t1, t2) = (Util.rev_apply r1 r_to_t, Util.rev_apply r2 r_to_t)
               in
             let (ks1, ks2) =
               (Util.rev_apply t1 Tree.tree_to_keys,
                 Util.rev_apply t2 Tree.tree_to_keys)
               in
             Key_value.check_keys l ks1 (Some ka) &&
               (Key_value.check_keys (Some ka) ks2 ua &&
                 List.equal_lista
                   (Product_Type.equal_prod Key_value_types.equal_key
                     Key_value_types.equal_value_t)
                   (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                     (Key_value.kvs_insert (k, v)))
                   (Util.rev_apply t1 Tree.tree_to_kvs @
                     Util.rev_apply t2 Tree.tree_to_kvs))));;

let rec step_up
  u = (match u with (_, []) -> Util.impossible1 "insert, step_up"
        | (fo, x :: stk) ->
          let a = Tree_stack.dest_frame x in
          let (aa, b) = a in
          let (ks1, rs1) = aa in
          (fun (_, (ks2, rs2)) ->
            (match fo
              with I1 r ->
                Util.rev_apply
                  (Util.rev_apply
                    (Util.rev_apply
                      (Frame_types.Node_frame (ks1 @ ks2, rs1 @ [r] @ rs2))
                      Frame_types.frame_to_page)
                    Store.alloc)
                  (Monad.fmap (fun ra -> (I1 ra, stk)))
              | I2 (r1, (k, r2)) ->
                let ks = ks1 @ [k] @ ks2 in
                let rs = rs1 @ [r1; r2] @ rs2 in
                (match
                  Arith.less_eq_nat (List.size_list ks) Constants.max_node_keys
                  with true ->
                    Util.rev_apply
                      (Util.rev_apply
                        (Util.rev_apply (Frame_types.Node_frame (ks, rs))
                          Frame_types.frame_to_page)
                        Store.alloc)
                      (Monad.fmap (fun r -> (I1 r, stk)))
                  | false ->
                    let (ks_rs1, (ka, ks_rs2)) = Key_value.split_node (ks, rs)
                      in
                    Util.rev_apply
                      (Util.rev_apply
                        (Util.rev_apply (Frame_types.Node_frame ks_rs1)
                          Frame_types.frame_to_page)
                        Store.alloc)
                      (Monad.bind
                        (fun r1a ->
                          Util.rev_apply
                            (Util.rev_apply
                              (Util.rev_apply (Frame_types.Node_frame ks_rs2)
                                Frame_types.frame_to_page)
                              Store.alloc)
                            (Monad.fmap
                              (fun r2a -> (I2 (r1a, (ka, r2a)), stk))))))))
            b);;

let rec step_down
  d = let (fs, v) = d in
      Util.rev_apply (Find.find_step fs) (Monad.fmap (fun da -> (da, v)));;

let rec step_bottom
  d = let (fs, v) = d in
      (match Find.dest_f_finished fs
        with None -> Util.impossible1 "insert, step_bottom"
        | Some (r0, (k, (_, (kvs, stk)))) ->
          Util.rev_apply (Store.free (r0 :: Frame.r_stk_to_rs stk))
            (Monad.bind
              (fun _ ->
                let kvsa = Util.rev_apply kvs (Key_value.kvs_insert (k, v)) in
                let fo =
                  (match
                    Arith.less_eq_nat (List.size_list kvsa)
                      Constants.max_leaf_size
                    with true ->
                      Util.rev_apply
                        (Util.rev_apply
                          (Util.rev_apply (Frame_types.Leaf_frame kvsa)
                            Frame_types.frame_to_page)
                          Store.alloc)
                        (Monad.fmap (fun a -> I1 a))
                    | false ->
                      let (kvs1, (ka, kvs2)) = Key_value.split_leaf kvsa in
                      Util.rev_apply
                        (Util.rev_apply
                          (Util.rev_apply (Frame_types.Leaf_frame kvs1)
                            Frame_types.frame_to_page)
                          Store.alloc)
                        (Monad.bind
                          (fun r1 ->
                            Util.rev_apply
                              (Util.rev_apply
                                (Util.rev_apply (Frame_types.Leaf_frame kvs2)
                                  Frame_types.frame_to_page)
                                Store.alloc)
                              (Monad.fmap (fun r2 -> I2 (r1, (ka, r2)))))))
                  in
                Util.rev_apply fo (Monad.fmap (fun foa -> (foa, stk))))));;

let rec insert_step
  s = (match s
        with I_down d ->
          let (fs, _) = d in
          (match Find.dest_f_finished fs
            with None ->
              Util.rev_apply (step_down d) (Monad.fmap (fun a -> I_down a))
            | Some _ ->
              Util.rev_apply (step_bottom d) (Monad.fmap (fun a -> I_up a)))
        | I_up u ->
          (match u with (I1 r, []) -> Monad.return (I_finished r)
            | (I2 (r1, (k, r2)), []) ->
              Util.rev_apply
                (Util.rev_apply
                  (Util.rev_apply (Frame_types.Node_frame ([k], [r1; r2]))
                    Frame_types.frame_to_page)
                  Store.alloc)
                (Monad.fmap (fun a -> I_finished a))
            | (_, _ :: _) ->
              Util.rev_apply (step_up u) (Monad.fmap (fun a -> I_up a)))
        | I_finished _ -> Monad.return s);;

let rec dest_i_finished
  s = (match s with I_down _ -> None | I_up _ -> None
        | I_finished a -> Some a);;

let rec mk_insert_state k v r = I_down (Find.mk_find_state k r, v);;

let rec wellformed_insert_state
  t0 k v s is =
    Util.assert_truea
      (match is with I_down a -> wf_d t0 s a | I_up a -> wf_u t0 k v s a
        | I_finished a -> wf_f t0 k v s a);;

end;;

module Insert_many : sig
  type i_t =
    I1 of (Store.page_ref *
            (Key_value_types.key * Key_value_types.value_t) list)
    | I2 of ((Store.page_ref * (Key_value_types.key * Store.page_ref)) *
              (Key_value_types.key * Key_value_types.value_t) list) [@@deriving yojson]
  type i_state_t =
    I_down of
      (Find.find_state *
        (Key_value_types.value_t *
          (Key_value_types.key * Key_value_types.value_t) list))
    | I_up of (i_t * (Store.page_ref, unit) Tree_stack.frame_ext list) |
    I_finished of
      (Store.page_ref * (Key_value_types.key * Key_value_types.value_t) list)
  val insert_step : i_state_t -> (i_state_t, Store.store) Monad.m_t
  val dest_i_finished :
    i_state_t ->
      (Store.page_ref *
        (Key_value_types.key * Key_value_types.value_t) list) option
  val mk_insert_state :
    Key_value_types.key ->
      Key_value_types.value_t ->
        (Key_value_types.key * Key_value_types.value_t) list ->
          Store.page_ref -> i_state_t
end = struct

type i_t =
  I1 of (Store.page_ref * (Key_value_types.key * Key_value_types.value_t) list)
  | I2 of ((Store.page_ref * (Key_value_types.key * Store.page_ref)) *
            (Key_value_types.key * Key_value_types.value_t) list) [@@deriving yojson];;

type i_state_t =
  I_down of
    (Find.find_state *
      (Key_value_types.value_t *
        (Key_value_types.key * Key_value_types.value_t) list))
  | I_up of (i_t * (Store.page_ref, unit) Tree_stack.frame_ext list) |
  I_finished of
    (Store.page_ref * (Key_value_types.key * Key_value_types.value_t) list) [@@deriving yojson];;

let rec step_up
  u = (match u with (_, []) -> Util.impossible1 "insert, step_up"
        | (fo, x :: stk) ->
          let a = Tree_stack.dest_frame x in
          let (aa, b) = a in
          let (ks1, rs1) = aa in
          (fun (_, (ks2, rs2)) ->
            (match fo
              with I1 (r, kvs0) ->
                Util.rev_apply
                  (Util.rev_apply
                    (Util.rev_apply
                      (Frame_types.Node_frame (ks1 @ ks2, rs1 @ [r] @ rs2))
                      Frame_types.frame_to_page)
                    Store.alloc)
                  (Monad.fmap (fun ra -> (I1 (ra, kvs0), stk)))
              | I2 ((r1, (k, r2)), kvs0) ->
                let ks = ks1 @ [k] @ ks2 in
                let rs = rs1 @ [r1; r2] @ rs2 in
                (match
                  Arith.less_eq_nat (List.size_list ks) Constants.max_node_keys
                  with true ->
                    Util.rev_apply
                      (Util.rev_apply
                        (Util.rev_apply (Frame_types.Node_frame (ks, rs))
                          Frame_types.frame_to_page)
                        Store.alloc)
                      (Monad.fmap (fun r -> (I1 (r, kvs0), stk)))
                  | false ->
                    let (ks_rs1, (ka, ks_rs2)) = Key_value.split_node (ks, rs)
                      in
                    Util.rev_apply
                      (Util.rev_apply
                        (Util.rev_apply (Frame_types.Node_frame ks_rs1)
                          Frame_types.frame_to_page)
                        Store.alloc)
                      (Monad.bind
                        (fun r1a ->
                          Util.rev_apply
                            (Util.rev_apply
                              (Util.rev_apply (Frame_types.Node_frame ks_rs2)
                                Frame_types.frame_to_page)
                              Store.alloc)
                            (Monad.fmap
                              (fun r2a ->
                                (I2 ((r1a, (ka, r2a)), kvs0), stk))))))))
            b);;

let rec step_down
  d = let (fs, v) = d in
      Util.rev_apply (Find.find_step fs) (Monad.fmap (fun da -> (da, v)));;

let rec split_leaf
  kvs = let n1 = List.size_list kvs in
        let n2 = Arith.zero_nat in
        let delta = Constants.min_leaf_size in
        let n1a = Arith.minus_nat n1 delta in
        let n2a = Arith.plus_nat n2 delta in
        let deltaa = Arith.minus_nat n1a Constants.max_leaf_size in
        let n1b = Arith.minus_nat n1a deltaa in
        let _ = Arith.plus_nat n2a deltaa in
        let (l, r) = Util.split_at n1b kvs in
        let k =
          (match r with [] -> Util.impossible1 "insert_many: split_leaf"
            | (k, _) :: _ -> k)
          in
        (l, (k, r));;

let rec kvs_insert_2
  u kv newa existing =
    let step =
      (fun (acc, newb) ->
        (match
          Arith.less_eq_nat
            (Arith.times_nat (Arith.nat_of_integer (Big_int.big_int_of_int 2))
              Constants.max_leaf_size)
            (List.size_list acc)
          with true -> None
          | false ->
            (match newb with [] -> None
              | (k, v) :: newc ->
                (match
                  Key_value.check_keys None
                    (Set.insert Key_value_types.equal_key k Set.bot_set) u
                  with true -> Some (Key_value.kvs_insert (k, v) acc, newc)
                  | false -> None))))
      in
    Util.iter_step step (existing, newa);;

let rec step_bottom
  d = let (fs, (v, kvs0)) = d in
      (match Find.dest_f_finished fs
        with None -> Util.impossible1 "insert, step_bottom"
        | Some (r0, (k, (_, (kvs, stk)))) ->
          Util.rev_apply (Store.free (r0 :: Frame.r_stk_to_rs stk))
            (Monad.bind
              (fun _ ->
                let (_, u) = Tree_stack.stack_to_lu_of_child stk in
                let (kvsa, kvs0a) = kvs_insert_2 u (k, v) kvs0 kvs in
                let fo =
                  (match
                    Arith.less_eq_nat (List.size_list kvsa)
                      Constants.max_leaf_size
                    with true ->
                      Util.rev_apply
                        (Util.rev_apply
                          (Util.rev_apply (Frame_types.Leaf_frame kvsa)
                            Frame_types.frame_to_page)
                          Store.alloc)
                        (Monad.fmap (fun r -> I1 (r, kvs0a)))
                    | false ->
                      let (kvs1, (ka, kvs2)) = split_leaf kvsa in
                      Util.rev_apply
                        (Util.rev_apply
                          (Util.rev_apply (Frame_types.Leaf_frame kvs1)
                            Frame_types.frame_to_page)
                          Store.alloc)
                        (Monad.bind
                          (fun r1 ->
                            Util.rev_apply
                              (Util.rev_apply
                                (Util.rev_apply (Frame_types.Leaf_frame kvs2)
                                  Frame_types.frame_to_page)
                                Store.alloc)
                              (Monad.fmap
                                (fun r2 -> I2 ((r1, (ka, r2)), kvs0a))))))
                  in
                Util.rev_apply fo (Monad.fmap (fun foa -> (foa, stk))))));;

let rec insert_step
  s = (match s
        with I_down d ->
          let (fs, (_, _)) = d in
          (match Find.dest_f_finished fs
            with None ->
              Util.rev_apply (step_down d) (Monad.fmap (fun a -> I_down a))
            | Some _ ->
              Util.rev_apply (step_bottom d) (Monad.fmap (fun a -> I_up a)))
        | I_up u ->
          (match u
            with (I1 (r, kvs0), []) -> Monad.return (I_finished (r, kvs0))
            | (I2 ((r1, (k, r2)), kvs0), []) ->
              Util.rev_apply
                (Util.rev_apply
                  (Util.rev_apply (Frame_types.Node_frame ([k], [r1; r2]))
                    Frame_types.frame_to_page)
                  Store.alloc)
                (Monad.fmap (fun r -> I_finished (r, kvs0)))
            | (_, _ :: _) ->
              Util.rev_apply (step_up u) (Monad.fmap (fun a -> I_up a)))
        | I_finished _ -> Monad.return s);;

let rec dest_i_finished
  s = (match s with I_down _ -> None | I_up _ -> None
        | I_finished (r, kvs) -> Some (r, kvs));;

let rec mk_insert_state k v kvs r = I_down (Find.mk_find_state k r, (v, kvs));;

end;;

module Leaf_stream : sig
  type ls_state
  val lss_step : ls_state -> (ls_state, Store.store) Monad.m_t
  val mk_ls_state : Store.page_ref -> ls_state
  val dest_LS_leaf :
    ls_state -> ((Key_value_types.key * Key_value_types.value_t) list) option
  val lss_is_finished : ls_state -> bool
end = struct

type ls_state =
  LS_down of (Store.page_ref * (Store.page_ref, unit) Tree_stack.frame_ext list)
  | LS_leaf of
      ((Key_value_types.key * Key_value_types.value_t) list *
        (Store.page_ref, unit) Tree_stack.frame_ext list)
  | LS_up of (Store.page_ref, unit) Tree_stack.frame_ext list;;

let rec step_up
  fs = let _ = Util.assert_true () (not (List.null fs)) in
       (match fs with [] -> Util.failwitha "impossible: Leaf_stream.step_up"
         | f :: fsa ->
           let a = Util.rev_apply f Tree_stack.dest_frame in
           let (aa, b) = a in
           let (ks1, rs1) = aa in
           (fun ab ->
             (match ab with (_, (_, [])) -> LS_up fsa
               | (r, (ks2, ra :: rs)) ->
                 let fa =
                   Tree_stack.Frame_ext
                     (ks1 @ [List.hd ks2], rs1 @ [r], ra, List.tl ks2, rs, ())
                   in
                 LS_down (ra, fa :: fsa)))
             b);;

let rec step_leaf r = let a = r in
                      let (_, aa) = a in
                      LS_up aa;;

let rec step_down
  rfs = let (r, fs) = rfs in
        Util.rev_apply (Frame.page_ref_to_frame r)
          (Monad.fmap
            (fun a ->
              (match a
                with Frame_types.Node_frame (ks, rs) ->
                  let ra = List.hd rs in
                  let rsa = List.tl rs in
                  let frm = Tree_stack.Frame_ext ([], [], ra, ks, rsa, ()) in
                  LS_down (ra, frm :: fs)
                | Frame_types.Leaf_frame kvs -> LS_leaf (kvs, fs))));;

let rec lss_step
  lss = (match lss with LS_down a -> step_down a
          | LS_leaf x -> Monad.return (step_leaf x)
          | LS_up x -> Monad.return (step_up x));;

let rec mk_ls_state r = LS_down (r, []);;

let rec dest_LS_leaf
  x = (match x with LS_down _ -> None | LS_leaf (kvs, _) -> Some kvs
        | LS_up _ -> None);;

let rec lss_is_finished
  lss = (match lss with LS_down _ -> false | LS_leaf _ -> false
          | LS_up [] -> true | LS_up (_ :: _) -> false);;


end;;


end;; (* functor *)
