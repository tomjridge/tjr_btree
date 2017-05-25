(** This file is exported from Isabelle, and lightly patched (eg to
   include this comment!). The OCaml interfaces wrap this basic
   functionality. *)

module Fun : sig
  val id : 'a -> 'a
  val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
end = struct

let rec id x = (fun xa -> xa) x;;

let rec comp f g = (fun x -> f (g x));;

end;; (*struct Fun*)

module Orderings : sig
  type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool}
  val less_eq : 'a ord -> 'a -> 'a -> bool
  val less : 'a ord -> 'a -> 'a -> bool
  val max : 'a ord -> 'a -> 'a -> 'a
end = struct

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

let rec max _A a b = (if less_eq _A a b then b else a);;

end;; (*struct Orderings*)

module Arith : sig
  type nat
  val less_eq_nat : nat -> nat -> bool
  val less_nat : nat -> nat -> bool
  val ord_nat : nat Orderings.ord
  type int = Int_of_integer of Big_int.big_int
  type num = One | Bit0 of num | Bit1 of num
  val plus_nat : nat -> nat -> nat
  val one_nat : nat
  val suc : nat -> nat
  val less_int : int -> int -> bool
  val zero_int : int
  val zero_nat : nat
  val nat_of_integer : Big_int.big_int -> nat
  val equal_int : int -> int -> bool
  val less_eq_int : int -> int -> bool
  val equal_nat : nat -> nat -> bool
  val minus_nat : nat -> nat -> nat
  val times_nat : nat -> nat -> nat
end = struct

type nat = Nat of Big_int.big_int;;

let rec integer_of_nat (Nat x) = x;;

let rec less_eq_nat
  m n = Big_int.le_big_int (integer_of_nat m) (integer_of_nat n);;

let rec less_nat
  m n = Big_int.lt_big_int (integer_of_nat m) (integer_of_nat n);;

let ord_nat =
  ({Orderings.less_eq = less_eq_nat; Orderings.less = less_nat} :
    nat Orderings.ord);;

let ord_integer =
  ({Orderings.less_eq = Big_int.le_big_int; Orderings.less = Big_int.lt_big_int}
    : Big_int.big_int Orderings.ord);;

type int = Int_of_integer of Big_int.big_int;;

type num = One | Bit0 of num | Bit1 of num;;

let rec plus_nat
  m n = Nat (Big_int.add_big_int (integer_of_nat m) (integer_of_nat n));;

let one_nat : nat = Nat (Big_int.big_int_of_int 1);;

let rec suc n = plus_nat n one_nat;;

let rec integer_of_int (Int_of_integer k) = k;;

let rec less_int
  k l = Big_int.lt_big_int (integer_of_int k) (integer_of_int l);;

let zero_int : int = Int_of_integer Big_int.zero_big_int;;

let zero_nat : nat = Nat Big_int.zero_big_int;;

let rec nat_of_integer
  k = Nat (Orderings.max ord_integer Big_int.zero_big_int k);;

let rec equal_int
  k l = Big_int.eq_big_int (integer_of_int k) (integer_of_int l);;

let rec less_eq_int
  k l = Big_int.le_big_int (integer_of_int k) (integer_of_int l);;

let rec equal_nat
  m n = Big_int.eq_big_int (integer_of_nat m) (integer_of_nat n);;

let rec minus_nat
  m n = Nat (Orderings.max ord_integer Big_int.zero_big_int
              (Big_int.sub_big_int (integer_of_nat m) (integer_of_nat n)));;

let rec times_nat
  m n = Nat (Big_int.mult_big_int (integer_of_nat m) (integer_of_nat n));;

end;; (*struct Arith*)

module List : sig
  val nth : 'a list -> Arith.nat -> 'a
  val upt : Arith.nat -> Arith.nat -> Arith.nat list
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val drop : Arith.nat -> 'a list -> 'a list
  val find : ('a -> bool) -> 'a list -> 'a option
  val null : 'a list -> bool
  val last : 'a list -> 'a
  val take : Arith.nat -> 'a list -> 'a list
  val foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val concat : ('a list) list -> 'a list
  val filter : ('a -> bool) -> 'a list -> 'a list
  val butlast : 'a list -> 'a list
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val list_ex : ('a -> bool) -> 'a list -> bool
  val map : ('a -> 'b) -> 'a list -> 'b list
  val pred_list : ('a -> bool) -> 'a list -> bool
  val size_list : 'a list -> Arith.nat
end = struct

let rec nth
  (x :: xs) n =
    (if Arith.equal_nat n Arith.zero_nat then x
      else nth xs (Arith.minus_nat n Arith.one_nat));;

let rec upt
  i j = (if Arith.less_nat i j then i :: upt (Arith.suc i) j else []);;

let rec zip
  xs ys = match xs, ys with x :: xs, y :: ys -> (x, y) :: zip xs ys
    | xs, [] -> []
    | [], ys -> [];;

let rec drop
  n x1 = match n, x1 with n, [] -> []
    | n, x :: xs ->
        (if Arith.equal_nat n Arith.zero_nat then x :: xs
          else drop (Arith.minus_nat n Arith.one_nat) xs);;

let rec find
  uu x1 = match uu, x1 with uu, [] -> None
    | p, x :: xs -> (if p x then Some x else find p xs);;

let rec null = function [] -> true
               | x :: xs -> false;;

let rec last (x :: xs) = (if null xs then x else last xs);;

let rec take
  n x1 = match n, x1 with n, [] -> []
    | n, x :: xs ->
        (if Arith.equal_nat n Arith.zero_nat then []
          else x :: take (Arith.minus_nat n Arith.one_nat) xs);;

let rec foldr
  f x1 = match f, x1 with f, [] -> Fun.id
    | f, x :: xs -> Fun.comp (f x) (foldr f xs);;

let rec concat xss = foldr (fun a b -> a @ b) xss [];;

let rec filter
  p x1 = match p, x1 with p, [] -> []
    | p, x :: xs -> (if p x then x :: filter p xs else filter p xs);;

let rec butlast
  = function [] -> []
    | x :: xs -> (if null xs then [] else x :: butlast xs);;

let rec hd (x21 :: x22) = x21;;

let rec tl = function [] -> []
             | x21 :: x22 -> x22;;

let rec list_ex
  p x1 = match p, x1 with p, [] -> false
    | p, x :: xs -> p x || list_ex p xs;;

let rec map
  f x1 = match f, x1 with f, [] -> []
    | f, x21 :: x22 -> f x21 :: map f x22;;

let rec pred_list
  p x1 = match p, x1 with p, [] -> true
    | p, x :: xs -> p x && pred_list p xs;;

let rec gen_length
  n x1 = match n, x1 with n, x :: xs -> gen_length (Arith.suc n) xs
    | n, [] -> n;;

let rec size_list x = gen_length Arith.zero_nat x;;

end;; (*struct List*)

module Set : sig
  type 'a set = Set of 'a list | Coset of 'a list
  val ball : 'a set -> ('a -> bool) -> bool
end = struct

type 'a set = Set of 'a list | Coset of 'a list;;

let rec ball (Set xs) p = List.pred_list p xs;;

end;; (*struct Set*)

module Product_Type : sig
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
end = struct

let rec fst (x1, x2) = x1;;

let rec snd (x1, x2) = x2;;

end;; (*struct Product_Type*)

module Prelude : sig
  type min_size_t = Small_root_node_or_leaf | Small_node | Small_leaf
  type 'a constants_ext =
    Constants_ext of Arith.nat * Arith.nat * Arith.nat * Arith.nat * 'a
  val max_leaf_size : 'a constants_ext -> Arith.nat
  val max_node_keys : 'a constants_ext -> Arith.nat
  val min_leaf_size : 'a constants_ext -> Arith.nat
  val min_node_keys : 'a constants_ext -> Arith.nat
end = struct

type min_size_t = Small_root_node_or_leaf | Small_node | Small_leaf;;

type 'a constants_ext =
  Constants_ext of Arith.nat * Arith.nat * Arith.nat * Arith.nat * 'a;;

let rec max_leaf_size
  (Constants_ext
    (min_leaf_size, max_leaf_size, min_node_keys, max_node_keys, more))
    = max_leaf_size;;

let rec max_node_keys
  (Constants_ext
    (min_leaf_size, max_leaf_size, min_node_keys, max_node_keys, more))
    = max_node_keys;;

let rec min_leaf_size
  (Constants_ext
    (min_leaf_size, max_leaf_size, min_node_keys, max_node_keys, more))
    = min_leaf_size;;

let rec min_node_keys
  (Constants_ext
    (min_leaf_size, max_leaf_size, min_node_keys, max_node_keys, more))
    = min_node_keys;;

end;; (*struct Prelude*)

module String : sig
  type nibble = Nibble0 | Nibble1 | Nibble2 | Nibble3 | Nibble4 | Nibble5 |
    Nibble6 | Nibble7 | Nibble8 | Nibble9 | NibbleA | NibbleB | NibbleC |
    NibbleD | NibbleE | NibbleF
end = struct

type nibble = Nibble0 | Nibble1 | Nibble2 | Nibble3 | Nibble4 | Nibble5 |
  Nibble6 | Nibble7 | Nibble8 | Nibble9 | NibbleA | NibbleB | NibbleC | NibbleD
  | NibbleE | NibbleF;;

end;; (*struct String*)

module Option : sig
  val is_none : 'a option -> bool
end = struct

let rec is_none = function Some x -> false
                  | None -> true;;

end;; (*struct Option*)

module Res = struct
  include Pervasives
  type 'a res = ('a,string) result
end

module Util : sig
  include module type of Res
  type error = String_error of string
  val rev_apply : 'a -> ('a -> 'b) -> 'b
  val unzip : ('a * 'b) list -> 'a list * 'b list
  val from_to : Arith.nat -> Arith.nat -> Arith.nat list
  val is_None : 'a option -> bool
  val failwitha : string -> 'a
  val assert_true : bool -> bool
  val split_at : Arith.nat -> 'a list -> 'a list * 'a list
  val dest_Some : 'a option -> 'a
  val dest_list : 'a list -> 'a * 'a list
  val iter_step : ('a -> 'a option) -> 'a -> 'a
  val dest_lista : 'a list -> 'a list * 'a
  val split_at_3 : Arith.nat -> 'a list -> 'a list * ('a * 'a list)
  val impossible1 : string -> 'a
  val max_of_list : Arith.nat list -> Arith.nat
end = struct

type error = String_error of string;;

include Res

let rec rev_apply x f = f x;;

let rec unzip
  xs = (rev_apply xs (List.map Product_Type.fst),
         rev_apply xs (List.map Product_Type.snd));;

let rec from_to x y = List.upt x (Arith.suc y);;

let rec is_None x = Option.is_none x;;

let rec failwitha x = failwith x

let rec assert_true b = (if b then b else failwitha "assert_true");;

let rec split_at
  n xs =
    (let _ = assert_true (Arith.less_eq_nat n (List.size_list xs)) in
     List.take n xs,
      List.drop n xs);;

let rec dest_Some = function Some x -> x
                    | None -> failwith "undefined";;

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
    let _ =
      assert_true
        (Arith.less_eq_nat n
          (Arith.minus_nat (List.size_list xs) Arith.one_nat))
      in
    (List.take n xs,
      (List.nth xs n, List.drop (Arith.plus_nat n Arith.one_nat) xs));;

let rec impossible1 x = failwitha x;;

let rec max_of_list
  xs = List.foldr (Orderings.max Arith.ord_nat) xs Arith.zero_nat;;

end;; (*struct Util*)

module Key_value : sig
  val key_eq : ('a -> 'a -> Arith.int) -> 'a -> 'a -> bool
  val key_lt : ('a -> 'a -> Arith.int) -> 'a -> 'a -> bool
  val kvs_equal : ('a * 'b) list -> ('a * 'b) list -> bool
  val check_keys :
    ('a -> 'a -> Arith.int) -> 'a option -> 'a Set.set -> 'a option -> bool
  val kvs_delete :
    ('a -> 'a -> Arith.int) -> 'a -> ('a * 'b) list -> ('a * 'b) list
  val kvs_insert :
    ('a -> 'a -> Arith.int) -> 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
  val split_leaf :
    unit Prelude.constants_ext ->
      ('a * 'b) list -> ('a * 'b) list * ('a * ('a * 'b) list)
  val split_node :
    unit Prelude.constants_ext ->
      'a list * 'b list -> ('a list * 'b list) * ('a * ('a list * 'b list))
  val split_ks_rs :
    ('a -> 'a -> Arith.int) ->
      'a -> 'a list * 'b list ->
              ('a list * 'b list) * ('b * ('a list * 'b list))
  val ordered_key_list : ('a -> 'a -> Arith.int) -> 'a list -> bool
end = struct

let rec key_eq ord k1 k2 = Arith.equal_int (ord k1 k2) Arith.zero_int;;

let rec key_le ord k1 k2 = Arith.less_eq_int (ord k1 k2) Arith.zero_int;;

let rec key_lt ord k1 k2 = Arith.less_int (ord k1 k2) Arith.zero_int;;

let rec kvs_equal x y = (x=y)

let rec check_keys
  cmp kl ks kr =
    let b1 =
      (match kl with None -> true | Some kla -> Set.ball ks (key_le cmp kla)) in
    let a =
      (match kr with None -> true
        | Some kra -> Set.ball ks (fun k -> key_lt cmp k kra))
      in
    b1 && a;;

let rec kvs_delete
  ord k kvs =
    List.filter (fun kv -> not (key_eq ord (Product_Type.fst kv) k)) kvs;;

let rec kvs_insert
  cmp kv x2 = match cmp, kv, x2 with cmp, kv, [] -> [kv]
    | cmp, kva, kv :: kvs ->
        let (k, v) = kva in
        let (ka, va) = kv in
        (if key_lt cmp ka k then (ka, va) :: kvs_insert cmp kva kvs
          else (if key_eq cmp k ka then (k, v) :: kvs
                 else (k, v) :: (ka, va) :: kvs));;

let rec split_leaf
  c kvs =
    let _ =
      Util.assert_true
        (Arith.less_eq_nat
          (Arith.plus_nat (Util.rev_apply c Prelude.max_leaf_size)
            Arith.one_nat)
          (List.size_list kvs))
      in
    let cut_point =
      Arith.minus_nat
        (Arith.plus_nat (Util.rev_apply c Prelude.max_leaf_size) Arith.one_nat)
        (Util.rev_apply c Prelude.min_leaf_size)
      in
    let _ = Util.assert_true (Arith.less_eq_nat cut_point (List.size_list kvs))
      in
    let (l, r) = Util.split_at cut_point kvs in
    let _ =
      Util.assert_true
        (Arith.less_eq_nat (Util.rev_apply c Prelude.min_leaf_size)
           (List.size_list l) &&
          Arith.less_eq_nat (Util.rev_apply c Prelude.min_leaf_size)
            (List.size_list r))
      in
    let k =
      (match r with [] -> Util.impossible1 "key_value, split_leaf"
        | (k, _) :: _ -> k)
      in
    (l, (k, r));;

let rec split_node
  c n = let (ks, rs) = n in
        let cut_point =
          Arith.minus_nat (Util.rev_apply c Prelude.max_node_keys)
            (Util.rev_apply c Prelude.min_node_keys)
          in
        let (ks1, (k, ks2)) = Util.split_at_3 cut_point ks in
        let _ =
          Util.assert_true
            (Arith.less_eq_nat (Util.rev_apply c Prelude.min_node_keys)
              (List.size_list ks2))
          in
        let (rs1, rs2) =
          Util.split_at (Arith.plus_nat cut_point Arith.one_nat) rs in
        ((ks1, rs1), (k, (ks2, rs2)));;

let rec search_key_to_index
  cmp ks k =
    let num_keys = List.size_list ks in
    let i =
      List.find (fun x -> key_lt cmp k (List.nth ks x))
        (List.upt Arith.zero_nat num_keys)
      in
    let ia = (match i with None -> num_keys | Some x -> x) in
    ia;;

let rec split_ks_rs
  cmp k ks_rs =
    let (ks, rs) = ks_rs in
    let _ =
      Util.assert_true
        (Arith.equal_nat (List.size_list rs)
          (Arith.plus_nat (List.size_list ks) Arith.one_nat))
      in
    let i = search_key_to_index cmp ks k in
    let _ = Util.assert_true (Arith.less_eq_nat i (List.size_list ks)) in
    let (ks1, ks2) = Util.split_at i ks in
    let _ =
      Util.assert_true
        (Arith.less_eq_nat i
          (Arith.minus_nat (List.size_list rs) Arith.one_nat))
      in
    let (rs1, (r, rs2)) = Util.split_at_3 i rs in
    ((ks1, rs1), (r, (ks2, rs2)));;

let rec ordered_key_list
  ord ks =
    Arith.less_nat (List.size_list ks)
      (Arith.nat_of_integer (Big_int.big_int_of_int 2)) ||
      List.pred_list
        (fun i ->
          key_lt ord (List.nth ks i)
            (List.nth ks (Arith.plus_nat i Arith.one_nat)))
        (Util.from_to Arith.zero_nat
          (Arith.minus_nat (List.size_list ks)
            (Arith.nat_of_integer (Big_int.big_int_of_int 2))));;

end;; (*struct Key_value*)

module Tree : sig
  type ('a, 'b) tree = Node of ('a list * ('a, 'b) tree list) |
    Leaf of ('a * 'b) list [@@deriving yojson]
  val height : ('a, 'b) tree -> Arith.nat
  val dest_Node : ('a, 'b) tree -> 'a list * ('a, 'b) tree list
  val tree_equal : ('a, 'b) tree -> ('a, 'b) tree -> bool
  val tree_to_leaves : ('a, 'b) tree -> (('a * 'b) list) list
  val tree_to_kvs : ('a, 'b) tree -> ('a * 'b) list
  val tree_to_keys : ('a, 'b) tree -> 'a Set.set
  val wellformed_tree :
    unit Prelude.constants_ext ->
      Prelude.min_size_t option ->
        ('a -> 'a -> Arith.int) -> ('a, 'b) tree -> bool
end = struct

type ('a, 'b) tree = Node of ('a list * ('a, 'b) tree list) |
  Leaf of ('a * 'b) list  [@@deriving yojson];;

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
           Arith.plus_nat Arith.one_nat (Util.max_of_list (List.map height cs))
         | Leaf _ -> Arith.one_nat);;

let rec forall_subtrees
  p t = List.pred_list p (Util.rev_apply t tree_to_subtrees);;

let rec get_min_size
  c mt =
    let min_leaf_size = Util.rev_apply c Prelude.min_leaf_size in
    let min_node_keys = Util.rev_apply c Prelude.min_node_keys in
    (match mt with (Prelude.Small_root_node_or_leaf, Node _) -> Arith.one_nat
      | (Prelude.Small_root_node_or_leaf, Leaf _) -> Arith.zero_nat
      | (Prelude.Small_node, Node _) ->
        Arith.minus_nat min_node_keys Arith.one_nat
      | (Prelude.Small_node, Leaf _) -> Util.failwitha "get_min_size"
      | (Prelude.Small_leaf, Node _) -> Util.failwitha "get_min_size"
      | (Prelude.Small_leaf, Leaf _) ->
        Arith.minus_nat min_leaf_size Arith.one_nat);;

let rec wf_size_1
  c t1 =
    (match t1
      with Node (l, _) ->
        let n = List.size_list l in
        Arith.less_eq_nat Arith.one_nat n &&
          (Arith.less_eq_nat (Util.rev_apply c Prelude.min_node_keys) n &&
            Arith.less_eq_nat n (Util.rev_apply c Prelude.max_node_keys))
      | Leaf xs ->
        let n = List.size_list xs in
        Arith.less_eq_nat (Util.rev_apply c Prelude.min_leaf_size) n &&
          Arith.less_eq_nat n (Util.rev_apply c Prelude.max_leaf_size));;

let rec wf_size
  c ms t0 =
    Util.assert_true
      (match ms with None -> forall_subtrees (wf_size_1 c) t0
        | Some m ->
          let min = get_min_size c (m, t0) in
          (match t0
            with Node (l, cs) ->
              let n = List.size_list l in
              Arith.less_eq_nat min n &&
                (Arith.less_eq_nat n (Util.rev_apply c Prelude.max_node_keys) &&
                  List.pred_list (forall_subtrees (wf_size_1 c)) cs)
            | Leaf xs ->
              let n = List.size_list xs in
              Arith.less_eq_nat min n &&
                Arith.less_eq_nat n (Util.rev_apply c Prelude.max_leaf_size)));;

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

let rec balanced t = Util.assert_true (forall_subtrees balanced_1 t);;

let rec wf_ks_rs_1
  t0 = (match t0
         with Node (l, cs) ->
           Arith.equal_nat (Arith.plus_nat Arith.one_nat (List.size_list l))
             (List.size_list cs)
         | Leaf _ -> true);;

let rec wf_ks_rs t0 = Util.assert_true (forall_subtrees wf_ks_rs_1 t0);;

let rec dest_Node
  = function Node (ks, rs) -> (ks, rs)
    | Leaf uu -> Util.failwitha "dest_Node";;

let rec tree_equal t1 t2 = (t1=t2)

let rec tree_to_leaves
  t0 = (match t0
         with Node (_, cs) ->
           Util.rev_apply (List.map tree_to_leaves cs) List.concat
         | Leaf l -> [l]);;

let rec tree_to_kvs
  t = Util.rev_apply (Util.rev_apply t tree_to_leaves) List.concat;;

let rec keys_ordered_1
  cmp t0 =
    Util.rev_apply (Util.rev_apply t0 keys_1) (Key_value.ordered_key_list cmp);;

let rec keys_ordered
  cmp t = Util.assert_true (forall_subtrees (keys_ordered_1 cmp) t);;

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
    Util.from_to min_child_index (ks_to_max_child_index ks);;

let rec keys_consistent_1
  cmp t0 =
    (match t0
      with Node (ks, rs) ->
        List.pred_list
          (fun i ->
            let a = index_to_bound ks i in
            let (l, aa) = a in
            Key_value.check_keys cmp l (Set.Set (keys (List.nth rs i))) aa)
          (subtree_indexes (ks, rs))
      | Leaf _ -> true);;

let rec keys_consistent
  cmp t = Util.assert_true (forall_subtrees (keys_consistent_1 cmp) t);;

let rec wellformed_tree
  c ms cmp t0 =
    Util.assert_true
      (let b1 = wf_size c ms t0 in
       let b2 = wf_ks_rs t0 in
       let b3 = balanced t0 in
       let b4 = keys_consistent cmp t0 in
       let b5 = keys_ordered cmp t0 in
       let wf = b1 && (b2 && (b3 && (b4 && b5))) in
       wf);;

end;; (*struct Tree*)

module Tree_stack : sig
  type ('a, 'b, 'c) ts_frame_ext =
    Ts_frame_ext of 'a list * 'b list * 'b * 'a list * 'b list * 'c [@@deriving yojson]
  val stack_map :
    ('a -> 'b) ->
      ('c, 'a, unit) ts_frame_ext list -> ('c, 'b, unit) ts_frame_ext list
  val no_focus :
    ('a, 'b, unit) ts_frame_ext list ->
      ('a, ('b option), unit) ts_frame_ext list
  val r_stk_to_rs : ('a, 'b, unit) ts_frame_ext list -> 'b list
  val stack_equal :
    ('a, 'b, unit) ts_frame_ext list -> ('a, 'b, unit) ts_frame_ext list -> bool
  val dest_ts_frame :
    ('a, 'b, unit) ts_frame_ext ->
      ('a list * 'b list) * ('b * ('a list * 'b list))
  val tree_to_stack :
    ('a -> 'a -> Arith.int) ->
      'a -> ('a, 'b) Tree.tree ->
              Arith.nat ->
                ('a, 'b) Tree.tree *
                  ('a, ('a, 'b) Tree.tree, unit) ts_frame_ext list
  val add_new_stack_frame :
    ('a -> 'a -> Arith.int) ->
      'a -> 'a list * 'b list ->
              ('a, 'b, unit) ts_frame_ext list ->
                ('a, 'b, unit) ts_frame_ext list * 'b
  val stack_to_lu_of_child :
    ('a, 'b, unit) ts_frame_ext list -> 'a option * 'a option
end = struct

type ('a, 'b, 'c) ts_frame_ext =
  Ts_frame_ext of 'a list * 'b list * 'b * 'a list * 'b list * 'c [@@deriving yojson];;

let rec f_t_update
  f_ta (Ts_frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) =
    Ts_frame_ext (f_ks1, f_ts1, f_ta f_t, f_ks2, f_ts2, more);;

let rec f_ts2 (Ts_frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ts2;;

let rec f_ts1 (Ts_frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ts1;;

let rec f_ks2 (Ts_frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ks2;;

let rec f_ks1 (Ts_frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_ks1;;

let rec f_t (Ts_frame_ext (f_ks1, f_ts1, f_t, f_ks2, f_ts2, more)) = f_t;;

let rec ts_frame_map
  g f = Ts_frame_ext
          (Util.rev_apply f f_ks1,
            Util.rev_apply (Util.rev_apply f f_ts1) (List.map g),
            Util.rev_apply (Util.rev_apply f f_t) g, Util.rev_apply f f_ks2,
            Util.rev_apply (Util.rev_apply f f_ts2) (List.map g), ());;

let rec stack_map f stk = Util.rev_apply stk (List.map (ts_frame_map f));;

let rec no_focus
  stk = Util.rev_apply (Util.rev_apply stk (stack_map (fun a -> Some a)))
          (fun a ->
            (match a with [] -> []
              | frm :: aa -> f_t_update (fun _ -> None) frm :: aa));;

let rec r_stk_to_rs xs = Util.rev_apply xs (List.map f_t);;

let rec stack_equal s1 s2 = (s1=s2)

let rec dest_ts_frame
  f = ((Util.rev_apply f f_ks1, Util.rev_apply f f_ts1),
        (Util.rev_apply f f_t,
          (Util.rev_apply f f_ks2, Util.rev_apply f f_ts2)));;

let rec tree_to_stack
  ord k t n =
    (if Arith.equal_nat n Arith.zero_nat then (t, [])
      else (match tree_to_stack ord k t (Arith.minus_nat n Arith.one_nat)
             with (Tree.Node (ks, ts), stk) ->
               let a = Key_value.split_ks_rs ord k (ks, ts) in
               let (aa, b) = a in
               let (ks1, ts1) = aa in
               (fun (ta, (ks2, ts2)) ->
                 let frm = Ts_frame_ext (ks1, ts1, ta, ks2, ts2, ()) in
                 (ta, frm :: stk))
                 b
             | (Tree.Leaf _, _) -> Util.failwitha "tree_to_stack"));;

let rec add_new_stack_frame
  cmp k ks_rs stk =
    let (ks, rs) = ks_rs in
    let a = Key_value.split_ks_rs cmp k (ks, rs) in
    let (aa, b) = a in
    let (ks1, rs1) = aa in
    (fun (r, (ks2, rs2)) ->
      let frm = Ts_frame_ext (ks1, rs1, r, ks2, rs2, ()) in
      (frm :: stk, r))
      b;;

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

end;; (*struct Tree_stack*)

module Frame : sig
  type ('a, 'b, 'c) frame = Node_frame of ('a list * 'c list) |
    Leaf_frame of ('a * 'b) list
  val dest_Leaf_frame : ('a, 'b, 'c) frame -> ('a * 'b) list
  val dest_Node_frame : ('a, 'b, 'c) frame -> 'a list * 'c list
end = struct

type ('a, 'b, 'c) frame = Node_frame of ('a list * 'c list) |
  Leaf_frame of ('a * 'b) list;;

let rec dest_Leaf_frame
  f = (match f with Node_frame _ -> Util.failwitha "dest_Leaf_frame"
        | Leaf_frame x -> x);;

let rec dest_Node_frame
  f = (match f with Node_frame x -> x
        | Leaf_frame _ -> Util.failwitha "dest_Node_frame");;

end;; (*struct Frame*)

module Pre_params : sig
  val dummy : unit
  val mk_r2t :
    ('a -> 'b -> ('c, 'd, 'b) Frame.frame option) ->
      Arith.nat -> 'a -> 'b -> ('c, 'd) Tree.tree option
end = struct

let dummy : unit = ();;

let rec mk_r2ta
  r2f n t r =
    (if Arith.equal_nat n Arith.zero_nat then None
      else (match r2f t r with None -> None
             | Some (Frame.Node_frame (ks, rs)) ->
               let ts =
                 List.map (mk_r2ta r2f (Arith.minus_nat n Arith.one_nat) t) rs
                 in
               (match List.filter Util.is_None ts
                 with [] ->
                   Some (Tree.Node
                          (ks, Util.rev_apply ts (List.map Util.dest_Some)))
                 | _ :: _ -> None)
             | Some (Frame.Leaf_frame kvs) -> Some (Tree.Leaf kvs)));;

let rec mk_r2t x = mk_r2ta x;;

end;; (*struct Pre_params*)

module Params : sig
  type 'a ps0 = Ps0 of (unit Prelude.constants_ext * ('a -> 'a -> Arith.int))
  type ('a, 'b, 'c, 'd, 'e) store_ops_ext =
    Store_ops_ext of
      ('c -> 'd -> 'd * ('a, 'b, 'c) Frame.frame Util.res) *
        (('a, 'b, 'c) Frame.frame -> 'd -> 'd * 'c Util.res) *
        ('c list -> 'd -> 'd * unit Util.res) * 'e
  type ('a, 'b, 'c, 'd) ps1 =
    Ps1 of ('a ps0 * ('a, 'b, 'c, 'd, unit) store_ops_ext)
  val ps0_cs : 'a ps0 -> unit Prelude.constants_ext
  val cs : ('a, 'b, 'c, 'd) ps1 -> unit Prelude.constants_ext
  val ps0_cmp_k : 'a ps0 -> 'a -> 'a -> Arith.int
  val cmp_k : ('a, 'b, 'c, 'd) ps1 -> 'a -> 'a -> Arith.int
  val dummy : unit
  val ps1_ps0 : ('a, 'b, 'c, 'd) ps1 -> 'a ps0
  val ps1_store_ops :
    ('a, 'b, 'c, 'd) ps1 -> ('a, 'b, 'c, 'd, unit) store_ops_ext
  val store_free :
    ('a, 'b, 'c, 'd, 'e) store_ops_ext -> 'c list -> 'd -> 'd * unit Util.res
  val store_read :
    ('a, 'b, 'c, 'd, 'e) store_ops_ext ->
      'c -> 'd -> 'd * ('a, 'b, 'c) Frame.frame Util.res
  val store_alloc :
    ('a, 'b, 'c, 'd, 'e) store_ops_ext ->
      ('a, 'b, 'c) Frame.frame -> 'd -> 'd * 'c Util.res
end = struct

type 'a ps0 = Ps0 of (unit Prelude.constants_ext * ('a -> 'a -> Arith.int));;

type ('a, 'b, 'c, 'd, 'e) store_ops_ext =
  Store_ops_ext of
    ('c -> 'd -> 'd * ('a, 'b, 'c) Frame.frame Util.res) *
      (('a, 'b, 'c) Frame.frame -> 'd -> 'd * 'c Util.res) *
      ('c list -> 'd -> 'd * unit Util.res) * 'e;;

type ('a, 'b, 'c, 'd) ps1 =
  Ps1 of ('a ps0 * ('a, 'b, 'c, 'd, unit) store_ops_ext);;

let rec dest_ps1 ps1 = let Ps1 a = ps1 in
                       let (aa, b) = a in
                       (aa, b);;

let rec dest_ps0 ps0 = let Ps0 a = ps0 in
                       let (aa, b) = a in
                       (aa, b);;

let rec ps0_cs
  ps0 = Util.rev_apply (Util.rev_apply ps0 dest_ps0) Product_Type.fst;;

let rec cs
  ps1 = Util.rev_apply
          (Util.rev_apply (Util.rev_apply ps1 dest_ps1) Product_Type.fst)
          ps0_cs;;

let rec ps0_cmp_k
  ps0 = Util.rev_apply (Util.rev_apply ps0 dest_ps0) Product_Type.snd;;

let rec cmp_k
  ps1 = Util.rev_apply
          (Util.rev_apply (Util.rev_apply ps1 dest_ps1) Product_Type.fst)
          ps0_cmp_k;;

let dummy : unit = Pre_params.dummy;;

let rec ps1_ps0
  ps1 = Util.rev_apply (Util.rev_apply ps1 dest_ps1) Product_Type.fst;;

let rec ps1_store_ops
  ps1 = Util.rev_apply (Util.rev_apply ps1 dest_ps1) Product_Type.snd;;

let rec store_free
  (Store_ops_ext (store_read, store_alloc, store_free, more)) = store_free;;

let rec store_read
  (Store_ops_ext (store_read, store_alloc, store_free, more)) = store_read;;

let rec store_alloc
  (Store_ops_ext (store_read, store_alloc, store_free, more)) = store_alloc;;

end;; (*struct Params*)

module Monad : sig
  val bind :
    ('a -> 'b -> 'b * 'c Util.res) ->
      ('b -> 'b * 'a Util.res) -> 'b -> 'b * 'c Util.res
  val fmap : ('a -> 'b) -> ('c -> 'c * 'a Util.res) -> 'c -> 'c * 'b Util.res
  val return : 'a -> 'b -> 'b * 'a Util.res
end = struct

let rec bind
  f m = (fun s ->
          Util.rev_apply (m s)
            (fun a ->
              (match a with (s1, Util.Ok y) -> f y s1
                | (s1, Util.Error x) -> (s1, Util.Error x))));;

let rec fmap
  f m = (fun s ->
          Util.rev_apply (m s)
            (fun (sa, r) ->
              (sa, (match r with Util.Ok y -> Util.Ok (f y)
                     | Util.Error a -> Util.Error a))));;

let rec return x = (fun s -> (s, Util.Ok x));;

end;; (*struct Monad*)

module Find : sig
  type ('a, 'b, 'c) find_state [@@deriving yojson]
  val find_step :
    ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) find_state -> 'd -> 'd * ('a, 'b, 'c) find_state Util.res
  val mk_find_state : 'a -> 'b -> ('a, 'c, 'b) find_state
  val wf_store_tree :
    ('a -> 'b -> ('c, 'd) Tree.tree option) ->
      'a -> 'b -> ('c, 'd) Tree.tree -> bool
  val dest_f_finished :
    ('a, 'b, 'c) find_state ->
      ('c * ('a * ('c * (('a * 'b) list *
                          ('a, 'c, unit) Tree_stack.ts_frame_ext list)))) option
  val wellformed_find_state :
    ('a -> 'a -> Arith.int) ->
      ('b -> 'c -> ('a, 'd) Tree.tree option) ->
        ('a, 'd) Tree.tree -> 'b -> ('a, 'd, 'c) find_state -> bool
end = struct

type ('a, 'b, 'c) find_state =
  F_down of ('c * ('a * ('c * ('a, 'c, unit) Tree_stack.ts_frame_ext list))) |
  F_finished of
    ('c * ('a * ('c * (('a * 'b) list *
                        ('a, 'c, unit) Tree_stack.ts_frame_ext list)))) [@@deriving yojson];;

let rec find_step
  ps1 fs =
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    (match fs
      with F_down (r0, (k, (r, stk))) ->
        Util.rev_apply (Util.rev_apply store_ops Params.store_read r)
          (Monad.fmap
            (fun a ->
              (match a
                with Frame.Node_frame (ks, rs) ->
                  let (stka, ra) =
                    Tree_stack.add_new_stack_frame
                      (Util.rev_apply ps1 Params.cmp_k) k (ks, rs) stk
                    in
                  F_down (r0, (k, (ra, stka)))
                | Frame.Leaf_frame kvs ->
                  F_finished (r0, (k, (r, (kvs, stk)))))))
      | F_finished _ -> Monad.return fs);;

let rec mk_find_state k r = F_down (r, (k, (r, [])));;

let rec wf_store_tree
  r2t s r t =
    Util.assert_true
      (match r2t s r with None -> false | Some a -> Tree.tree_equal t a);;

let rec dest_f_finished
  fs = (match fs with F_down _ -> None
         | F_finished (r0, (k, (r, (kvs, stk)))) ->
           Some (r0, (k, (r, (kvs, stk)))));;

let rec wellformed_find_state
  k_ord r2t t0 s fs =
    Util.assert_true
      (let _ = Tree.height t0 in
       let check_focus = wf_store_tree r2t s in
       let check_stack =
         (fun rstk tstk ->
           Tree_stack.stack_equal
             (Util.rev_apply tstk (Tree_stack.stack_map (fun a -> Some a)))
             (Util.rev_apply rstk (Tree_stack.stack_map (r2t s))))
         in
       (match fs
         with F_down (_, (k, (r, stk))) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_focus r t_fo) &&
             Util.assert_true (check_stack stk t_stk)
         | F_finished (_, (k, (r, (_, stk)))) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_focus r t_fo) &&
             Util.assert_true (check_stack stk t_stk)));;

end;; (*struct Find*)

module Delete : sig
  type ('a, 'b, 'c) del_t = D_small_leaf of ('a * 'b) list |
    D_small_node of ('a list * 'c list) | D_updated_subtree of 'c  [@@deriving yojson]
  type ('a, 'b, 'c) delete_state = D_down of (('a, 'b, 'c) Find.find_state * 'c)
    | D_up of
        (('a, 'b, 'c) del_t *
          (('a, 'c, unit) Tree_stack.ts_frame_ext list * 'c))
    | D_finished of 'c  [@@deriving yojson]
  val delete_step :
    ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) delete_state -> 'd -> 'd * ('a, 'b, 'c) delete_state Util.res
  val dest_d_finished : ('a, 'b, 'c) delete_state -> 'c option
  val mk_delete_state : 'a -> 'b -> ('a, 'c, 'b) delete_state
  val wellformed_delete_state :
    'a Params.ps0 ->
      ('b -> 'c -> ('a, 'd) Tree.tree option) ->
        ('a, 'd) Tree.tree -> 'b -> 'a -> ('a, 'd, 'c) delete_state -> bool
end = struct

type ('a, 'b) d12_t = D1 of 'b | D2 of ('b * ('a * 'b))  [@@deriving yojson];;

type ('a, 'b, 'c) del_t = D_small_leaf of ('a * 'b) list |
  D_small_node of ('a list * 'c list) | D_updated_subtree of 'c  [@@deriving yojson];;

type ('a, 'b, 'c) delete_state = D_down of (('a, 'b, 'c) Find.find_state * 'c) |
  D_up of
    (('a, 'b, 'c) del_t * (('a, 'c, unit) Tree_stack.ts_frame_ext list * 'c))
  | D_finished of 'c  [@@deriving yojson];;

let rec wf_d
  k_ord r2f t0 s d =
    Util.assert_true
      (let (fs, _) = d in
       Util.assert_true (Find.wellformed_find_state k_ord r2f t0 s fs));;

let rec wf_f
  ps0 r2t t0 s k r =
    Util.assert_true
      (let (constants, k_ord) =
         (Util.rev_apply ps0 Params.ps0_cs, Util.rev_apply ps0 Params.ps0_cmp_k)
         in
       let t = Util.rev_apply (r2t s r) Util.dest_Some in
       Util.assert_true
         (Tree.wellformed_tree constants (Some Prelude.Small_root_node_or_leaf)
           k_ord t) &&
         Util.assert_true
           (Key_value.kvs_equal
             (Util.rev_apply (Util.rev_apply t0 Tree.tree_to_kvs)
               (Key_value.kvs_delete k_ord k))
             (Util.rev_apply t Tree.tree_to_kvs)));;

let rec wf_u
  ps0 r2t t0 s k u =
    Util.assert_true
      (let (constants, k_ord) =
         (Util.rev_apply ps0 Params.ps0_cs, Util.rev_apply ps0 Params.ps0_cmp_k)
         in
       let (fo, stk) = u in
       let check_stack =
         (fun rstk tstk ->
           Tree_stack.stack_equal
             (Util.rev_apply
               (Util.rev_apply rstk (Tree_stack.stack_map (r2t s)))
               Tree_stack.no_focus)
             (Util.rev_apply
               (Util.rev_apply tstk (Tree_stack.stack_map (fun a -> Some a)))
               Tree_stack.no_focus))
         in
       let check_wf = (fun ms -> Tree.wellformed_tree constants ms k_ord) in
       let check_focus =
         (fun foa ->
           Key_value.kvs_equal
             (Util.rev_apply (Util.rev_apply foa Tree.tree_to_kvs)
               (Key_value.kvs_delete k_ord k)))
         in
       (match fo
         with D_small_leaf kvs ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k_ord k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Prelude.Small_root_node_or_leaf
               | _ :: _ -> Some Prelude.Small_leaf)
             in
           Util.assert_true (check_stack stk t_stk) &&
             (Util.assert_true (check_wf ms (Tree.Leaf kvs)) &&
               Util.assert_true (check_focus t_fo kvs))
         | D_small_node (ks, rs) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k_ord k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Prelude.Small_root_node_or_leaf
               | _ :: _ -> Some Prelude.Small_node)
             in
           let t =
             Tree.Node
               (ks, Util.rev_apply (Util.rev_apply rs (List.map (r2t s)))
                      (List.map Util.dest_Some))
             in
           Util.assert_true (check_stack stk t_stk) &&
             (Util.assert_true (check_wf ms t) &&
               Util.assert_true
                 (check_focus t_fo (Util.rev_apply t Tree.tree_to_kvs)))
         | D_updated_subtree r ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k_ord k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Prelude.Small_root_node_or_leaf
               | _ :: _ -> None)
             in
           let t = Util.rev_apply (Util.rev_apply r (r2t s)) Util.dest_Some in
           Util.assert_true (check_stack stk t_stk) &&
             (Util.assert_true (check_wf ms t) &&
               Util.assert_true
                 (check_focus t_fo (Util.rev_apply t Tree.tree_to_kvs)))));;

let rec frac_mult
  xs ys =
    let a = xs in
    let (aa, b) = a in
    let (aaa, ba) = ys in
    (aa @ aaa, b @ ba);;

let rec post_steal_or_merge
  ps1 stk p_unused p_1 p_2 x =
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    let m = frac_mult in
    (match x
      with D1 c ->
        let p = Frame.Node_frame (m (m p_1 ([], [c])) p_2) in
        let p_sz =
          Util.rev_apply
            (Util.rev_apply (Util.rev_apply p Frame.dest_Node_frame)
              Product_Type.fst)
            List.size_list
          in
        let f =
          (match Arith.equal_nat p_sz Arith.zero_nat
            with true ->
              let _ = Util.assert_true (List.null stk) in
              Monad.return (D_updated_subtree c)
            | false ->
              (match
                Arith.less_nat p_sz
                  (Util.rev_apply (Util.rev_apply ps1 Params.cs)
                    Prelude.min_node_keys)
                with true ->
                  Monad.return
                    (D_small_node (Util.rev_apply p Frame.dest_Node_frame))
                | false ->
                  Util.rev_apply
                    (Util.rev_apply p
                      (Util.rev_apply store_ops Params.store_alloc))
                    (Monad.fmap (fun a -> D_updated_subtree a))))
          in
        Util.rev_apply f (Monad.fmap (fun fa -> (fa, stk)))
      | D2 (c1, (k, c2)) ->
        let p = Frame.Node_frame (m (m p_1 ([k], [c1; c2])) p_2) in
        let p_sz =
          Util.rev_apply
            (Util.rev_apply (Util.rev_apply p Frame.dest_Node_frame)
              Product_Type.fst)
            List.size_list
          in
        let f =
          (match
            Arith.less_nat p_sz
              (Util.rev_apply (Util.rev_apply ps1 Params.cs)
                Prelude.min_node_keys)
            with true ->
              let _ = Util.assert_true (List.null stk) in
              Monad.return
                (D_small_node (Util.rev_apply p Frame.dest_Node_frame))
            | false ->
              Util.rev_apply
                (Util.rev_apply p (Util.rev_apply store_ops Params.store_alloc))
                (Monad.fmap (fun a -> D_updated_subtree a)))
          in
        Util.rev_apply f (Monad.fmap (fun fa -> (fa, stk))));;

let rec steal_or_merge
  constants right leaf mk_c c p_k s =
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
          (if leaf then Util.rev_apply constants Prelude.min_leaf_size
            else Util.rev_apply constants Prelude.min_node_keys)
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
  ps1 du =
    let (f, stk) = du in
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    (match stk with [] -> Util.impossible1 "delete, step_up"
      | p :: stka ->
        (match f
          with D_small_leaf kvs ->
            let leaf = true in
            let mk_c = (fun (ks, vs) -> Frame.Leaf_frame (List.zip ks vs)) in
            let a = Util.rev_apply p Tree_stack.dest_ts_frame in
            let (aa, b) = a in
            let (p_ks1, p_rs1) = aa in
            (fun (_, (p_ks2, p_rs2)) ->
              let ab = get_sibling ((p_ks1, p_rs1), (p_ks2, p_rs2)) in
              let (right, ac) = ab in
              let (ad, ba) = ac in
              let (p_1, p_2) = ad in
              (fun (p_k, r) ->
                let frm = Util.rev_apply store_ops Params.store_read r in
                let d12 =
                  Util.rev_apply frm
                    (Monad.fmap
                      (fun frma ->
                        steal_or_merge (Util.rev_apply ps1 Params.cs) right leaf
                          mk_c (Util.rev_apply kvs Util.unzip) p_k
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
                              (Util.rev_apply frma
                                (Util.rev_apply store_ops Params.store_alloc))
                              (Monad.fmap (fun af -> D1 af))
                          | D2 (frm1, (p_ka, frm2)) ->
                            Util.rev_apply
                              (Util.rev_apply frm1
                                (Util.rev_apply store_ops Params.store_alloc))
                              (Monad.bind
                                (fun r1 ->
                                  Util.rev_apply
                                    (Util.rev_apply frm2
                                      (Util.rev_apply store_ops
Params.store_alloc))
                                    (Monad.fmap
                                      (fun r2 -> D2 (r1, (p_ka, r2)))))))))
                  in
                Util.rev_apply d12a
                  (Monad.bind (post_steal_or_merge ps1 stka p p_1 p_2)))
                ba)
              b
          | D_small_node (ks, rs) ->
            let leaf = false in
            let mk_c = (fun a -> Frame.Node_frame a) in
            let a = Util.rev_apply p Tree_stack.dest_ts_frame in
            let (aa, b) = a in
            let (p_ks1, p_rs1) = aa in
            (fun (_, (p_ks2, p_rs2)) ->
              let ab = get_sibling ((p_ks1, p_rs1), (p_ks2, p_rs2)) in
              let (right, ac) = ab in
              let (ad, ba) = ac in
              let (p_1, p_2) = ad in
              (fun (p_k, r) ->
                let frm = Util.rev_apply store_ops Params.store_read r in
                let d12 =
                  Util.rev_apply frm
                    (Monad.fmap
                      (fun frma ->
                        steal_or_merge (Util.rev_apply ps1 Params.cs) right leaf
                          mk_c (ks, rs) p_k
                          (Util.rev_apply frma Frame.dest_Node_frame)))
                  in
                let d12a =
                  Util.rev_apply d12
                    (Monad.bind
                      (fun ae ->
                        (match ae
                          with D1 frma ->
                            Util.rev_apply
                              (Util.rev_apply frma
                                (Util.rev_apply store_ops Params.store_alloc))
                              (Monad.fmap (fun af -> D1 af))
                          | D2 (frm1, (p_ka, frm2)) ->
                            Util.rev_apply
                              (Util.rev_apply frm1
                                (Util.rev_apply store_ops Params.store_alloc))
                              (Monad.bind
                                (fun r1 ->
                                  Util.rev_apply
                                    (Util.rev_apply frm2
                                      (Util.rev_apply store_ops
Params.store_alloc))
                                    (Monad.fmap
                                      (fun r2 -> D2 (r1, (p_ka, r2)))))))))
                  in
                Util.rev_apply d12a
                  (Monad.bind (post_steal_or_merge ps1 stka p p_1 p_2)))
                ba)
              b
          | D_updated_subtree r ->
            let a = Util.rev_apply p Tree_stack.dest_ts_frame in
            let (aa, b) = a in
            let (ks1, rs1) = aa in
            (fun (_, (ks2, rs2)) ->
              Util.rev_apply
                (Util.rev_apply (Frame.Node_frame (ks1 @ ks2, rs1 @ [r] @ rs2))
                  (Util.rev_apply store_ops Params.store_alloc))
                (Monad.fmap (fun ra -> (D_updated_subtree ra, stka))))
              b));;

let rec delete_step
  ps1 s =
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    (match s
      with D_down (f, r0) ->
        (match Find.dest_f_finished f
          with None ->
            Util.rev_apply (Find.find_step ps1 f)
              (Monad.fmap (fun fa -> D_down (fa, r0)))
          | Some (r0a, (k, (_, (kvs, stk)))) ->
            Util.rev_apply
              (Util.rev_apply store_ops Params.store_free
                (r0a :: Tree_stack.r_stk_to_rs stk))
              (Monad.bind
                (fun _ ->
                  (match
                    List.list_ex
                      (fun x ->
                        Key_value.key_eq (Util.rev_apply ps1 Params.cmp_k) x k)
                      (Util.rev_apply kvs (List.map Product_Type.fst))
                    with true ->
                      let kvsa =
                        Util.rev_apply kvs
                          (List.filter
                            (fun x ->
                              not (Key_value.key_eq
                                    (Util.rev_apply ps1 Params.cmp_k)
                                    (Product_Type.fst x) k)))
                        in
                      (match
                        Arith.less_nat (List.size_list kvsa)
                          (Util.rev_apply (Util.rev_apply ps1 Params.cs)
                            Prelude.min_leaf_size)
                        with true ->
                          Monad.return (D_up (D_small_leaf kvsa, (stk, r0a)))
                        | false ->
                          Util.rev_apply
                            (Util.rev_apply (Frame.Leaf_frame kvsa)
                              (Util.rev_apply store_ops Params.store_alloc))
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
                  (Util.rev_apply (Frame.Leaf_frame kvs)
                    (Util.rev_apply store_ops Params.store_alloc))
                  (Monad.fmap (fun a -> D_finished a))
              | D_small_node (ks, rs) ->
                Util.rev_apply
                  (Util.rev_apply (Frame.Node_frame (ks, rs))
                    (Util.rev_apply store_ops Params.store_alloc))
                  (Monad.fmap (fun a -> D_finished a))
              | D_updated_subtree r -> Monad.return (D_finished r))
          | _ :: _ ->
            Util.rev_apply (step_up ps1 (f, stk))
              (Monad.fmap (fun (fa, stka) -> D_up (fa, (stka, r0)))))
      | D_finished _ -> Monad.return s);;

let rec dest_d_finished
  x = (match x with D_down _ -> None | D_up _ -> None
        | D_finished a -> Some a);;

let rec mk_delete_state k r = D_down (Find.mk_find_state k r, r);;

let rec wellformed_delete_state
  ps0 r2t t0 s k ds =
    Util.assert_true
      (let (_, k_ord) =
         (Util.rev_apply ps0 Params.ps0_cs, Util.rev_apply ps0 Params.ps0_cmp_k)
         in
       (match ds with D_down a -> wf_d k_ord r2t t0 s a
         | D_up (fo, (stk, r)) ->
           wf_u ps0 r2t t0 s k (fo, stk) &&
             (match r2t s r with None -> false | Some t -> Tree.tree_equal t t0)
         | D_finished a -> wf_f ps0 r2t t0 s k a));;

end;; (*struct Delete*)

module Insert : sig
  type ('a, 'b, 'c) i12_t = I1 of 'c | I2 of ('c * ('a * 'c)) [@@deriving yojson]
  type ('a, 'b, 'c) insert_state = I_down of (('a, 'b, 'c) Find.find_state * 'b)
    | I_up of (('a, 'b, 'c) i12_t * ('a, 'c, unit) Tree_stack.ts_frame_ext list)
    | I_finished of 'c  [@@deriving yojson]
  val insert_step :
    ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) insert_state -> 'd -> 'd * ('a, 'b, 'c) insert_state Util.res
  val dest_i_finished : ('a, 'b, 'c) insert_state -> 'c option
  val mk_insert_state : 'a -> 'b -> 'c -> ('a, 'b, 'c) insert_state
  val wellformed_insert_state :
    'a Params.ps0 ->
      ('b -> 'c -> ('a, 'd) Tree.tree option) ->
        ('a, 'd) Tree.tree ->
          'b -> 'a -> 'd -> ('a, 'd, 'c) insert_state -> bool
end = struct

type ('a, 'b, 'c) i12_t = I1 of 'c | I2 of ('c * ('a * 'c)) [@@deriving yojson];;

type ('a, 'b, 'c) insert_state = I_down of (('a, 'b, 'c) Find.find_state * 'b) |
  I_up of (('a, 'b, 'c) i12_t * ('a, 'c, unit) Tree_stack.ts_frame_ext list) |
  I_finished of 'c  [@@deriving yojson];;

let rec wf_d
  k_ord r2t t0 s d =
    Util.assert_true
      (let (fs, _) = d in
       Find.wellformed_find_state k_ord r2t t0 s fs);;

let rec wf_f
  ps0 r2t t0 s k v r =
    Util.assert_true
      (let (cs, k_ord) =
         (Util.rev_apply ps0 Params.ps0_cs, Util.rev_apply ps0 Params.ps0_cmp_k)
         in
       (match r2t s r with None -> false
         | Some t ->
           Tree.wellformed_tree cs (Some Prelude.Small_root_node_or_leaf) k_ord
             t &&
             Key_value.kvs_equal
               (Util.rev_apply (Util.rev_apply t0 Tree.tree_to_kvs)
                 (Key_value.kvs_insert k_ord (k, v)))
               (Util.rev_apply t Tree.tree_to_kvs)));;

let rec wf_u
  r2t k_ord t0 s k v u =
    Util.assert_true
      (let _ = Find.wf_store_tree r2t s in
       let check_stack =
         (fun rstk tstk ->
           Tree_stack.stack_equal
             (Util.rev_apply
               (Util.rev_apply rstk (Tree_stack.stack_map (r2t s)))
               Tree_stack.no_focus)
             (Util.rev_apply
               (Util.rev_apply tstk (Tree_stack.stack_map (fun a -> Some a)))
               Tree_stack.no_focus))
         in
       (match u
         with (I1 r, stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_stack stk t_stk) &&
             (match r2t s r with None -> Util.assert_true false
               | Some t ->
                 Util.assert_true
                   (Key_value.kvs_equal (Util.rev_apply t Tree.tree_to_kvs)
                     (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                       (Key_value.kvs_insert k_ord (k, v)))))
         | (I2 (r1, (ka, r2)), stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_stack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_stack stk t_stk) &&
             let (l, ua) = Tree_stack.stack_to_lu_of_child t_stk in
             (match (r2t s r1, r2t s r2) with (None, _) -> false
               | (Some _, None) -> false
               | (Some t1, Some t2) ->
                 let (ks1, ks2) =
                   (Util.rev_apply t1 Tree.tree_to_keys,
                     Util.rev_apply t2 Tree.tree_to_keys)
                   in
                 Util.assert_true
                   (Key_value.check_keys k_ord l ks1 (Some ka)) &&
                   (Util.assert_true
                      (Key_value.check_keys k_ord (Some ka) ks2 ua) &&
                     Util.assert_true
                       (Key_value.kvs_equal
                         (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                           (Key_value.kvs_insert k_ord (k, v)))
                         (Util.rev_apply t1 Tree.tree_to_kvs @
                           Util.rev_apply t2 Tree.tree_to_kvs))))));;

let rec step_up
  ps1 u =
    let (cs, _) =
      (Util.rev_apply ps1 Params.cs, Util.rev_apply ps1 Params.cmp_k) in
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    (match u with (_, []) -> Util.impossible1 "insert, step_up"
      | (fo, x :: stk) ->
        let a = Tree_stack.dest_ts_frame x in
        let (aa, b) = a in
        let (ks1, rs1) = aa in
        (fun (_, (ks2, rs2)) ->
          (match fo
            with I1 r ->
              Util.rev_apply
                (Util.rev_apply (Frame.Node_frame (ks1 @ ks2, rs1 @ [r] @ rs2))
                  (Util.rev_apply store_ops Params.store_alloc))
                (Monad.fmap (fun ra -> (I1 ra, stk)))
            | I2 (r1, (k, r2)) ->
              let ks = ks1 @ [k] @ ks2 in
              let rs = rs1 @ [r1; r2] @ rs2 in
              (match
                Arith.less_eq_nat (List.size_list ks)
                  (Util.rev_apply cs Prelude.max_node_keys)
                with true ->
                  Util.rev_apply
                    (Util.rev_apply (Frame.Node_frame (ks, rs))
                      (Util.rev_apply store_ops Params.store_alloc))
                    (Monad.fmap (fun r -> (I1 r, stk)))
                | false ->
                  let (ks_rs1, (ka, ks_rs2)) = Key_value.split_node cs (ks, rs)
                    in
                  Util.rev_apply
                    (Util.rev_apply (Frame.Node_frame ks_rs1)
                      (Util.rev_apply store_ops Params.store_alloc))
                    (Monad.bind
                      (fun r1a ->
                        Util.rev_apply
                          (Util.rev_apply (Frame.Node_frame ks_rs2)
                            (Util.rev_apply store_ops Params.store_alloc))
                          (Monad.fmap
                            (fun r2a -> (I2 (r1a, (ka, r2a)), stk))))))))
          b);;

let rec step_down
  ps1 d =
    let (fs, v) = d in
    Util.rev_apply (Find.find_step ps1 fs) (Monad.fmap (fun da -> (da, v)));;

let rec step_bottom
  ps1 d =
    let (cs, k_ord) =
      (Util.rev_apply ps1 Params.cs, Util.rev_apply ps1 Params.cmp_k) in
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    let (fs, v) = d in
    (match Find.dest_f_finished fs
      with None -> Util.impossible1 "insert, step_bottom"
      | Some (r0, (k, (_, (kvs, stk)))) ->
        Util.rev_apply
          (Util.rev_apply store_ops Params.store_free
            (r0 :: Tree_stack.r_stk_to_rs stk))
          (Monad.bind
            (fun _ ->
              let kvsa = Util.rev_apply kvs (Key_value.kvs_insert k_ord (k, v))
                in
              let fo =
                (match
                  Arith.less_eq_nat (List.size_list kvsa)
                    (Util.rev_apply cs Prelude.max_leaf_size)
                  with true ->
                    Util.rev_apply
                      (Util.rev_apply (Frame.Leaf_frame kvsa)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.fmap (fun a -> I1 a))
                  | false ->
                    let (kvs1, (ka, kvs2)) = Key_value.split_leaf cs kvsa in
                    Util.rev_apply
                      (Util.rev_apply (Frame.Leaf_frame kvs1)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.bind
                        (fun r1 ->
                          Util.rev_apply
                            (Util.rev_apply (Frame.Leaf_frame kvs2)
                              (Util.rev_apply store_ops Params.store_alloc))
                            (Monad.fmap (fun r2 -> I2 (r1, (ka, r2)))))))
                in
              Util.rev_apply fo (Monad.fmap (fun foa -> (foa, stk))))));;

let rec insert_step
  ps1 s =
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    (match s
      with I_down d ->
        let (fs, _) = d in
        (match Find.dest_f_finished fs
          with None ->
            Util.rev_apply (step_down ps1 d) (Monad.fmap (fun a -> I_down a))
          | Some _ ->
            Util.rev_apply (step_bottom ps1 d) (Monad.fmap (fun a -> I_up a)))
      | I_up u ->
        (match u with (I1 r, []) -> Monad.return (I_finished r)
          | (I2 (r1, (k, r2)), []) ->
            Util.rev_apply
              (Util.rev_apply (Frame.Node_frame ([k], [r1; r2]))
                (Util.rev_apply store_ops Params.store_alloc))
              (Monad.fmap (fun a -> I_finished a))
          | (_, _ :: _) ->
            Util.rev_apply (step_up ps1 u) (Monad.fmap (fun a -> I_up a)))
      | I_finished _ -> Monad.return s);;

let rec dest_i_finished
  s = (match s with I_down _ -> None | I_up _ -> None
        | I_finished a -> Some a);;

let rec mk_insert_state k v r = I_down (Find.mk_find_state k r, v);;

let rec wellformed_insert_state
  ps0 r2t t0 s k v is =
    Util.assert_true
      (let k_ord = Util.rev_apply ps0 Params.ps0_cmp_k in
       (match is with I_down a -> wf_d k_ord r2t t0 s a
         | I_up a -> wf_u r2t k_ord t0 s k v a
         | I_finished a -> wf_f ps0 r2t t0 s k v a));;

end;; (*struct Insert*)

module Insert_many : sig
  type ('a, 'b, 'c) fo = I1 of ('c * ('a * 'b) list) |
    I2 of (('c * ('a * 'c)) * ('a * 'b) list)
  type ('a, 'b, 'c) ist =
    I_down of (('a, 'b, 'c) Find.find_state * ('b * ('a * 'b) list)) |
    I_up of (('a, 'b, 'c) fo * ('a, 'c, unit) Tree_stack.ts_frame_ext list) |
    I_finished of ('c * ('a * 'b) list)
  val insert_step :
    ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) ist -> 'd -> 'd * ('a, 'b, 'c) ist Util.res
  val dest_i_finished : ('a, 'b, 'c) ist -> ('c * ('a * 'b) list) option
  val mk_insert_state : 'a -> 'b -> ('a * 'b) list -> 'c -> ('a, 'b, 'c) ist
end = struct

type ('a, 'b, 'c) fo = I1 of ('c * ('a * 'b) list) |
  I2 of (('c * ('a * 'c)) * ('a * 'b) list);;

type ('a, 'b, 'c) ist =
  I_down of (('a, 'b, 'c) Find.find_state * ('b * ('a * 'b) list)) |
  I_up of (('a, 'b, 'c) fo * ('a, 'c, unit) Tree_stack.ts_frame_ext list) |
  I_finished of ('c * ('a * 'b) list);;

let rec step_up
  ps1 u =
    let (cs, _) =
      (Util.rev_apply ps1 Params.cs, Util.rev_apply ps1 Params.cmp_k) in
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    (match u with (_, []) -> Util.impossible1 "insert, step_up"
      | (fo, x :: stk) ->
        let a = Tree_stack.dest_ts_frame x in
        let (aa, b) = a in
        let (ks1, rs1) = aa in
        (fun (_, (ks2, rs2)) ->
          (match fo
            with I1 (r, kvs0) ->
              Util.rev_apply
                (Util.rev_apply (Frame.Node_frame (ks1 @ ks2, rs1 @ [r] @ rs2))
                  (Util.rev_apply store_ops Params.store_alloc))
                (Monad.fmap (fun ra -> (I1 (ra, kvs0), stk)))
            | I2 ((r1, (k, r2)), kvs0) ->
              let ks = ks1 @ [k] @ ks2 in
              let rs = rs1 @ [r1; r2] @ rs2 in
              (match
                Arith.less_eq_nat (List.size_list ks)
                  (Util.rev_apply cs Prelude.max_node_keys)
                with true ->
                  Util.rev_apply
                    (Util.rev_apply (Frame.Node_frame (ks, rs))
                      (Util.rev_apply store_ops Params.store_alloc))
                    (Monad.fmap (fun r -> (I1 (r, kvs0), stk)))
                | false ->
                  let (ks_rs1, (ka, ks_rs2)) = Key_value.split_node cs (ks, rs)
                    in
                  Util.rev_apply
                    (Util.rev_apply (Frame.Node_frame ks_rs1)
                      (Util.rev_apply store_ops Params.store_alloc))
                    (Monad.bind
                      (fun r1a ->
                        Util.rev_apply
                          (Util.rev_apply (Frame.Node_frame ks_rs2)
                            (Util.rev_apply store_ops Params.store_alloc))
                          (Monad.fmap
                            (fun r2a ->
                              (I2 ((r1a, (ka, r2a)), kvs0), stk))))))))
          b);;

let rec step_down
  ps1 d =
    let (fs, v) = d in
    Util.rev_apply (Find.find_step ps1 fs) (Monad.fmap (fun da -> (da, v)));;

let rec split_leaf
  cs0 kvs =
    let n1 = List.size_list kvs in
    let n2 = Arith.zero_nat in
    let delta = Util.rev_apply cs0 Prelude.min_leaf_size in
    let n1a = Arith.minus_nat n1 delta in
    let n2a = Arith.plus_nat n2 delta in
    let deltaa = Arith.minus_nat n1a (Util.rev_apply cs0 Prelude.max_leaf_size)
      in
    let n1b = Arith.minus_nat n1a deltaa in
    let _ = Arith.plus_nat n2a deltaa in
    let (l, r) = Util.split_at n1b kvs in
    let k =
      (match r with [] -> Util.impossible1 "insert_many: split_leaf"
        | (k, _) :: _ -> k)
      in
    (l, (k, r));;

let rec kvs_insert_2
  ps0 u kv newa existing =
    let (cs, k_ord) =
      (Util.rev_apply ps0 Params.ps0_cs, Util.rev_apply ps0 Params.ps0_cmp_k) in
    let step =
      (fun (acc, newb) ->
        (match
          Arith.less_eq_nat
            (Arith.times_nat (Arith.nat_of_integer (Big_int.big_int_of_int 2))
              (Util.rev_apply cs Prelude.max_leaf_size))
            (List.size_list acc)
          with true -> None
          | false ->
            (match newb with [] -> None
              | (k, v) :: newc ->
                let test =
                  (fun ka a ->
                    (match a with None -> true
                      | Some aa -> Key_value.key_lt k_ord ka aa))
                  in
                (match test k u
                  with true ->
                    Some (Key_value.kvs_insert k_ord (k, v) acc, newc)
                  | false -> None))))
      in
    Util.iter_step step (existing, newa);;

let rec step_bottom
  ps1 d =
    let (cs, _) =
      (Util.rev_apply ps1 Params.cs, Util.rev_apply ps1 Params.cmp_k) in
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    let (fs, (v, kvs0)) = d in
    (match Find.dest_f_finished fs
      with None -> Util.impossible1 "insert, step_bottom"
      | Some (r0, (k, (_, (kvs, stk)))) ->
        Util.rev_apply
          (Util.rev_apply store_ops Params.store_free
            (r0 :: Tree_stack.r_stk_to_rs stk))
          (Monad.bind
            (fun _ ->
              let (_, u) = Tree_stack.stack_to_lu_of_child stk in
              let (kvsa, kvs0a) =
                kvs_insert_2 (Util.rev_apply ps1 Params.ps1_ps0) u (k, v) kvs0
                  kvs
                in
              let fo =
                (match
                  Arith.less_eq_nat (List.size_list kvsa)
                    (Util.rev_apply cs Prelude.max_leaf_size)
                  with true ->
                    Util.rev_apply
                      (Util.rev_apply (Frame.Leaf_frame kvsa)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.fmap (fun r -> I1 (r, kvs0a)))
                  | false ->
                    let (kvs1, (ka, kvs2)) = split_leaf cs kvsa in
                    Util.rev_apply
                      (Util.rev_apply (Frame.Leaf_frame kvs1)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.bind
                        (fun r1 ->
                          Util.rev_apply
                            (Util.rev_apply (Frame.Leaf_frame kvs2)
                              (Util.rev_apply store_ops Params.store_alloc))
                            (Monad.fmap
                              (fun r2 -> I2 ((r1, (ka, r2)), kvs0a))))))
                in
              Util.rev_apply fo (Monad.fmap (fun foa -> (foa, stk))))));;

let rec insert_step
  ps1 s =
    let (_, _) = (Util.rev_apply ps1 Params.cs, Util.rev_apply ps1 Params.cmp_k)
      in
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    (match s
      with I_down d ->
        let (fs, (_, _)) = d in
        (match Find.dest_f_finished fs
          with None ->
            Util.rev_apply (step_down ps1 d) (Monad.fmap (fun a -> I_down a))
          | Some _ ->
            Util.rev_apply (step_bottom ps1 d) (Monad.fmap (fun a -> I_up a)))
      | I_up u ->
        (match u with (I1 (r, kvs0), []) -> Monad.return (I_finished (r, kvs0))
          | (I2 ((r1, (k, r2)), kvs0), []) ->
            Util.rev_apply
              (Util.rev_apply (Frame.Node_frame ([k], [r1; r2]))
                (Util.rev_apply store_ops Params.store_alloc))
              (Monad.fmap (fun r -> I_finished (r, kvs0)))
          | (_, _ :: _) ->
            Util.rev_apply (step_up ps1 u) (Monad.fmap (fun a -> I_up a)))
      | I_finished _ -> Monad.return s);;

let rec dest_i_finished
  s = (match s with I_down _ -> None | I_up _ -> None
        | I_finished (r, kvs) -> Some (r, kvs));;

let rec mk_insert_state k v kvs r = I_down (Find.mk_find_state k r, (v, kvs));;

end;; (*struct Insert_many*)

module Leaf_stream : sig
  type ('a, 'b, 'c) ls_state
  val lss_step :
    ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) ls_state -> 'd -> 'd * ('a, 'b, 'c) ls_state Util.res
  val mk_ls_state : 'a -> ('b, 'c, 'a) ls_state
  val dest_LS_leaf : ('a, 'b, 'c) ls_state -> (('a * 'b) list) option
  val lss_is_finished : ('a, 'b, 'c) ls_state -> bool
end = struct

type ('a, 'b, 'c) ls_state =
  LS_down of ('c * ('a, 'c, unit) Tree_stack.ts_frame_ext list) |
  LS_leaf of (('a * 'b) list * ('a, 'c, unit) Tree_stack.ts_frame_ext list) |
  LS_up of ('a, 'c, unit) Tree_stack.ts_frame_ext list;;

let rec step_up
  fs = let _ = Util.assert_true (not (List.null fs)) in
       (match fs with [] -> Util.failwitha "impossible: Leaf_stream.step_up"
         | f :: fsa ->
           let a = Util.rev_apply f Tree_stack.dest_ts_frame in
           let (aa, b) = a in
           let (ks1, rs1) = aa in
           (fun ab ->
             (match ab with (_, (_, [])) -> LS_up fsa
               | (r, (ks2, ra :: rs)) ->
                 let fa =
                   Tree_stack.Ts_frame_ext
                     (ks1 @ [List.hd ks2], rs1 @ [r], ra, List.tl ks2, rs, ())
                   in
                 LS_down (ra, fa :: fsa)))
             b);;

let rec step_leaf r = let a = r in
                      let (_, aa) = a in
                      LS_up aa;;

let rec step_down
  ps1 rfs =
    let (r, fs) = rfs in
    let store_ops = Util.rev_apply ps1 Params.ps1_store_ops in
    Util.rev_apply (Util.rev_apply store_ops Params.store_read r)
      (Monad.fmap
        (fun a ->
          (match a
            with Frame.Node_frame (ks, rs) ->
              let ra = List.hd rs in
              let rsa = List.tl rs in
              let frm = Tree_stack.Ts_frame_ext ([], [], ra, ks, rsa, ()) in
              LS_down (ra, frm :: fs)
            | Frame.Leaf_frame kvs -> LS_leaf (kvs, fs))));;

let rec lss_step
  ps1 lss =
    (match lss with LS_down a -> step_down ps1 a
      | LS_leaf x -> Monad.return (step_leaf x)
      | LS_up x -> Monad.return (step_up x));;

let rec mk_ls_state r = LS_down (r, []);;

let rec dest_LS_leaf
  x = (match x with LS_down _ -> None | LS_leaf (kvs, _) -> Some kvs
        | LS_up _ -> None);;

let rec lss_is_finished
  lss = (match lss with LS_down _ -> false | LS_leaf _ -> false
          | LS_up [] -> true | LS_up (_ :: _) -> false);;

end;; (*struct Leaf_stream*)
