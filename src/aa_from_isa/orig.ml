module Fun : sig
  val id : 'a -> 'a
  val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
end = struct

let rec id x = (fun xa -> xa) x;;

let rec comp f g = (fun x -> f (g x));;

end;; (*struct Fun*)

module HOL : sig
  type 'a equal = {equal : 'a -> 'a -> bool}
  val equal : 'a equal -> 'a -> 'a -> bool
  val eq : 'a equal -> 'a -> 'a -> bool
end = struct

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

let rec eq _A a b = equal _A a b;;

end;; (*struct HOL*)

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
  val equal_nata : nat -> nat -> bool
  val equal_nat : nat HOL.equal
  val less_eq_nat : nat -> nat -> bool
  val less_nat : nat -> nat -> bool
  val ord_nat : nat Orderings.ord
  type int = Int_of_integer of Big_int.big_int
  type num = One | Bit0 of num | Bit1 of num
  val plus_nat : nat -> nat -> nat
  val one_nat : nat
  val suc : nat -> nat
  val less_int : int -> int -> bool
  val int_of_nat : nat -> int
  val zero_int : int
  val zero_nat : nat
  val nat_of_integer : Big_int.big_int -> nat
  val equal_int : int -> int -> bool
  val minus_int : int -> int -> int
  val less_eq_int : int -> int -> bool
  val minus_nat : nat -> nat -> nat
  val times_nat : nat -> nat -> nat
end = struct

type nat = Nat of Big_int.big_int;;

let rec integer_of_nat (Nat x) = x;;

let rec equal_nata
  m n = Big_int.eq_big_int (integer_of_nat m) (integer_of_nat n);;

let equal_nat = ({HOL.equal = equal_nata} : nat HOL.equal);;

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

let rec int_of_nat n = Int_of_integer (integer_of_nat n);;

let zero_int : int = Int_of_integer Big_int.zero_big_int;;

let zero_nat : nat = Nat Big_int.zero_big_int;;

let rec nat_of_integer
  k = Nat (Orderings.max ord_integer Big_int.zero_big_int k);;

let rec equal_int
  k l = Big_int.eq_big_int (integer_of_int k) (integer_of_int l);;

let rec minus_int
  k l = Int_of_integer
          (Big_int.sub_big_int (integer_of_int k) (integer_of_int l));;

let rec less_eq_int
  k l = Big_int.le_big_int (integer_of_int k) (integer_of_int l);;

let rec minus_nat
  m n = Nat (Orderings.max ord_integer Big_int.zero_big_int
              (Big_int.sub_big_int (integer_of_nat m) (integer_of_nat n)));;

let rec times_nat
  m n = Nat (Big_int.mult_big_int (integer_of_nat m) (integer_of_nat n));;

end;; (*struct Arith*)

module List : sig
  val equal_lista : 'a HOL.equal -> 'a list -> 'a list -> bool
  val equal_list : 'a HOL.equal -> ('a list) HOL.equal
  val nth : 'a list -> Arith.nat -> 'a
  val rev : 'a list -> 'a list
  val upt : Arith.nat -> Arith.nat -> Arith.nat list
  val drop : Arith.nat -> 'a list -> 'a list
  val null : 'a list -> bool
  val take : Arith.nat -> 'a list -> 'a list
  val foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val concat : ('a list) list -> 'a list
  val filter : ('a -> bool) -> 'a list -> 'a list
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val list_ex : ('a -> bool) -> 'a list -> bool
  val map : ('a -> 'b) -> 'a list -> 'b list
  val pred_list : ('a -> bool) -> 'a list -> bool
  val size_list : 'a list -> Arith.nat
end = struct

let rec equal_lista _A
  x0 x1 = match x0, x1 with [], x21 :: x22 -> false
    | x21 :: x22, [] -> false
    | x21 :: x22, y21 :: y22 -> HOL.eq _A x21 y21 && equal_lista _A x22 y22
    | [], [] -> true;;

let rec equal_list _A = ({HOL.equal = equal_lista _A} : ('a list) HOL.equal);;

let rec nth
  (x :: xs) n =
    (if Arith.equal_nata n Arith.zero_nat then x
      else nth xs (Arith.minus_nat n Arith.one_nat));;

let rec fold
  f x1 s = match f, x1, s with f, x :: xs, s -> fold f xs (f x s)
    | f, [], s -> s;;

let rec rev xs = fold (fun a b -> a :: b) xs [];;

let rec upt
  i j = (if Arith.less_nat i j then i :: upt (Arith.suc i) j else []);;

let rec drop
  n x1 = match n, x1 with n, [] -> []
    | n, x :: xs ->
        (if Arith.equal_nata n Arith.zero_nat then x :: xs
          else drop (Arith.minus_nat n Arith.one_nat) xs);;

let rec null = function [] -> true
               | x :: xs -> false;;

let rec take
  n x1 = match n, x1 with n, [] -> []
    | n, x :: xs ->
        (if Arith.equal_nata n Arith.zero_nat then []
          else x :: take (Arith.minus_nat n Arith.one_nat) xs);;

let rec foldr
  f x1 = match f, x1 with f, [] -> Fun.id
    | f, x :: xs -> Fun.comp (f x) (foldr f xs);;

let rec concat xss = foldr (fun a b -> a @ b) xss [];;

let rec filter
  p x1 = match p, x1 with p, [] -> []
    | p, x :: xs -> (if p x then x :: filter p xs else filter p xs);;

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
  val is_empty : 'a set -> bool
end = struct

type 'a set = Set of 'a list | Coset of 'a list;;

let rec ball (Set xs) p = List.pred_list p xs;;

let rec is_empty (Set xs) = List.null xs;;

end;; (*struct Set*)

module Product_Type : sig
  val equal_proda : 'a HOL.equal -> 'b HOL.equal -> 'a * 'b -> 'a * 'b -> bool
  val equal_prod : 'a HOL.equal -> 'b HOL.equal -> ('a * 'b) HOL.equal
  val fst : 'a * 'b -> 'a
end = struct

let rec equal_proda _A _B
  (x1, x2) (y1, y2) = HOL.eq _A x1 y1 && HOL.eq _B x2 y2;;

let rec equal_prod _A _B =
  ({HOL.equal = equal_proda _A _B} : ('a * 'b) HOL.equal);;

let rec fst (x1, x2) = x1;;

end;; (*struct Product_Type*)

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

module Util : sig
  type error = String_error of string
  type 'a res = Ok of 'a | Error of error
  val from_to : Arith.nat -> Arith.nat -> Arith.nat list
  val is_Nil : 'a list -> bool
  val is_None : 'a option -> bool
  val rev_apply : 'a -> ('a -> 'b) -> 'b
  val failwitha : string -> 'a
  val check_true : (unit -> bool) -> bool
  val split_at : Arith.nat -> 'a list -> 'a list * 'a list
  val dest_Some : 'a option -> 'a
  val iter_step : ('a -> 'a option) -> 'a -> 'a
  val split_at_3 : Arith.nat -> 'a list -> 'a list * ('a * 'a list)
  val assert_true : bool -> bool
  val impossible1 : string -> 'a
  val max_of_list : Arith.nat list -> Arith.nat
  val from_to_tests : unit
  val split_at_tests : unit
  val split_at_3_tests : unit
end = struct

type error = String_error of string;;

type 'a res = Ok of 'a | Error of error;;

let rec from_to x y = List.upt x (Arith.suc y);;

let rec is_Nil x = (match x with [] -> true | _ :: _ -> false);;

let rec is_None x = Option.is_none x;;

let rec rev_apply x f = f x;;

let rec failwitha x = rev_apply "FIXME patch" (fun _ -> failwith "undefined");;

let rec check_true f = rev_apply "FIXME patch" (fun _ -> failwith "undefined");;

let rec split_at
  n xs =
    (let _ = check_true (fun _ -> Arith.less_eq_nat n (List.size_list xs)) in
     List.take n xs,
      List.drop n xs);;

let rec dest_Some = function Some x -> x
                    | None -> failwith "undefined";;

let rec iter_step
  f x = let a = f x in
        (match a with None -> x | Some aa -> iter_step f aa);;

let rec split_at_3
  n xs =
    let _ = check_true (fun _ -> Arith.less_nat n (List.size_list xs)) in
    (List.take n xs,
      (List.nth xs n, List.drop (Arith.plus_nat n Arith.one_nat) xs));;

let rec assert_true b = (if b then b else failwitha "assert_true");;

let rec impossible1 x = failwitha x;;

let rec max_of_list
  xs = List.foldr (Orderings.max Arith.ord_nat) xs Arith.zero_nat;;

let from_to_tests : unit
  = let _ =
      assert_true
        (List.equal_lista Arith.equal_nat
          (from_to (Arith.nat_of_integer (Big_int.big_int_of_int 3))
            (Arith.nat_of_integer (Big_int.big_int_of_int 5)))
          [Arith.nat_of_integer (Big_int.big_int_of_int 3);
            Arith.nat_of_integer (Big_int.big_int_of_int 4);
            Arith.nat_of_integer (Big_int.big_int_of_int 5)])
      in
    let _ =
      assert_true
        (List.equal_lista Arith.equal_nat
          (from_to (Arith.nat_of_integer (Big_int.big_int_of_int 3))
            (Arith.nat_of_integer (Big_int.big_int_of_int 3)))
          [Arith.nat_of_integer (Big_int.big_int_of_int 3)])
      in
    let _ =
      assert_true
        (List.null
          (from_to (Arith.nat_of_integer (Big_int.big_int_of_int 3))
            (Arith.nat_of_integer (Big_int.big_int_of_int 2))))
      in
    ();;

let split_at_tests : unit
  = let _ =
      assert_true
        (Product_Type.equal_proda (List.equal_list Arith.equal_nat)
          (List.equal_list Arith.equal_nat)
          (split_at (Arith.nat_of_integer (Big_int.big_int_of_int 3))
            [Arith.zero_nat; Arith.one_nat;
              Arith.nat_of_integer (Big_int.big_int_of_int 2);
              Arith.nat_of_integer (Big_int.big_int_of_int 3);
              Arith.nat_of_integer (Big_int.big_int_of_int 4)])
          ([Arith.zero_nat; Arith.one_nat;
             Arith.nat_of_integer (Big_int.big_int_of_int 2)],
            [Arith.nat_of_integer (Big_int.big_int_of_int 3);
              Arith.nat_of_integer (Big_int.big_int_of_int 4)]))
      in
    let _ =
      assert_true
        (Product_Type.equal_proda (List.equal_list Arith.equal_nat)
          (List.equal_list Arith.equal_nat)
          (split_at (Arith.nat_of_integer (Big_int.big_int_of_int 3))
            [Arith.zero_nat; Arith.one_nat;
              Arith.nat_of_integer (Big_int.big_int_of_int 2)])
          ([Arith.zero_nat; Arith.one_nat;
             Arith.nat_of_integer (Big_int.big_int_of_int 2)],
            []))
      in
    ();;

let split_at_3_tests : unit
  = let _ =
      assert_true
        (Product_Type.equal_proda (List.equal_list Arith.equal_nat)
          (Product_Type.equal_prod Arith.equal_nat
            (List.equal_list Arith.equal_nat))
          (split_at_3 (Arith.nat_of_integer (Big_int.big_int_of_int 3))
            [Arith.zero_nat; Arith.one_nat;
              Arith.nat_of_integer (Big_int.big_int_of_int 2);
              Arith.nat_of_integer (Big_int.big_int_of_int 3);
              Arith.nat_of_integer (Big_int.big_int_of_int 4)])
          ([Arith.zero_nat; Arith.one_nat;
             Arith.nat_of_integer (Big_int.big_int_of_int 2)],
            (Arith.nat_of_integer (Big_int.big_int_of_int 3),
              [Arith.nat_of_integer (Big_int.big_int_of_int 4)])))
      in
    let _ =
      assert_true
        (Product_Type.equal_proda (List.equal_list Arith.equal_nat)
          (Product_Type.equal_prod Arith.equal_nat
            (List.equal_list Arith.equal_nat))
          (split_at_3 (Arith.nat_of_integer (Big_int.big_int_of_int 3))
            [Arith.zero_nat; Arith.one_nat;
              Arith.nat_of_integer (Big_int.big_int_of_int 2);
              Arith.nat_of_integer (Big_int.big_int_of_int 3)])
          ([Arith.zero_nat; Arith.one_nat;
             Arith.nat_of_integer (Big_int.big_int_of_int 2)],
            (Arith.nat_of_integer (Big_int.big_int_of_int 3), [])))
      in
    ();;

end;; (*struct Util*)

module Key_value : sig
  val key_eq : ('a -> 'a -> Arith.int) -> 'a -> 'a -> bool
  val key_lt : ('a -> 'a -> Arith.int) -> 'a -> 'a -> bool
  val check_keys :
    ('a -> 'a -> Arith.int) -> 'a option -> 'a Set.set -> 'a option -> bool
  val ck_tests : unit
  val ck2_tests : unit
  val kvs_equal : ('a * 'b) list -> ('a * 'b) list -> bool
  val ordered_key_list : ('a -> 'a -> Arith.int) -> 'a list -> bool
  val okl_tests : unit
  val kvs_delete :
    ('a -> 'a -> Arith.int) -> 'a -> ('a * 'b) list -> ('a * 'b) list
  val kvs_insert :
    ('a -> 'a -> Arith.int) -> 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
  val kvs_insert_tests : unit
end = struct

let rec key_eq ord k1 k2 = Arith.equal_int (ord k1 k2) Arith.zero_int;;

let rec key_le ord k1 k2 = Arith.less_eq_int (ord k1 k2) Arith.zero_int;;

let rec key_lt ord k1 k2 = Arith.less_int (ord k1 k2) Arith.zero_int;;

let rec nat_ord
  x y = let n2i = Arith.int_of_nat in
        Arith.minus_int (n2i x) (n2i y);;

let rec check_keys
  cmp kl ks kr =
    let b1 =
      (match kl with None -> true | Some kla -> Set.ball ks (key_le cmp kla)) in
    let a =
      (match kr with None -> true
        | Some kra -> Set.ball ks (fun k -> key_lt cmp k kra))
      in
    b1 && a;;

let ck_tests : unit
  = let _ =
      Util.assert_true
        (check_keys nat_ord (Some Arith.one_nat)
          (Set.Set
            [Arith.one_nat; Arith.nat_of_integer (Big_int.big_int_of_int 2);
              Arith.nat_of_integer (Big_int.big_int_of_int 3)])
          (Some (Arith.nat_of_integer (Big_int.big_int_of_int 4))))
      in
    let _ =
      Util.assert_true
        (not (check_keys nat_ord (Some Arith.one_nat)
               (Set.Set
                 [Arith.one_nat;
                   Arith.nat_of_integer (Big_int.big_int_of_int 2);
                   Arith.nat_of_integer (Big_int.big_int_of_int 3)])
               (Some (Arith.nat_of_integer (Big_int.big_int_of_int 3)))))
      in
    ();;

let rec check_keys_2
  cmp xs l ks u zs =
    (match Option.is_none l with true -> Set.is_empty xs | false -> true) &&
      ((match Option.is_none u with true -> Set.is_empty zs | false -> true) &&
        (check_keys cmp None xs l &&
          (check_keys cmp l ks u && check_keys cmp u zs None)));;

let ck2_tests : unit
  = let _ =
      Util.assert_true
        (check_keys_2 nat_ord (Set.Set [Arith.zero_nat]) (Some Arith.one_nat)
          (Set.Set
            [Arith.one_nat; Arith.nat_of_integer (Big_int.big_int_of_int 2);
              Arith.nat_of_integer (Big_int.big_int_of_int 3)])
          (Some (Arith.nat_of_integer (Big_int.big_int_of_int 4)))
          (Set.Set
            [Arith.nat_of_integer (Big_int.big_int_of_int 4);
              Arith.nat_of_integer (Big_int.big_int_of_int 5)]))
      in
    ();;

let rec kvs_equal x = Util.failwitha "FIXME patch" x;;

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

let okl_tests : unit
  = let _ =
      Util.assert_true
        (ordered_key_list nat_ord
          [Arith.zero_nat; Arith.one_nat;
            Arith.nat_of_integer (Big_int.big_int_of_int 2);
            Arith.nat_of_integer (Big_int.big_int_of_int 3)])
      in
    let _ =
      Util.assert_true
        (not (ordered_key_list nat_ord
               [Arith.zero_nat; Arith.one_nat; Arith.one_nat;
                 Arith.nat_of_integer (Big_int.big_int_of_int 3)]))
      in
    ();;

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

let kvs_insert_tests : unit
  = let _ =
      Util.assert_true
        (List.equal_lista
          (Product_Type.equal_prod Arith.equal_nat Arith.equal_nat)
          (kvs_insert nat_ord
            (Arith.nat_of_integer (Big_int.big_int_of_int 2),
              Arith.nat_of_integer (Big_int.big_int_of_int 2))
            (List.map (fun x -> (x, x))
              [Arith.zero_nat; Arith.one_nat;
                Arith.nat_of_integer (Big_int.big_int_of_int 3);
                Arith.nat_of_integer (Big_int.big_int_of_int 4)]))
          (List.map (fun x -> (x, x))
            [Arith.zero_nat; Arith.one_nat;
              Arith.nat_of_integer (Big_int.big_int_of_int 2);
              Arith.nat_of_integer (Big_int.big_int_of_int 3);
              Arith.nat_of_integer (Big_int.big_int_of_int 4)]))
      in
    let _ =
      Util.assert_true
        (List.equal_lista
          (Product_Type.equal_prod Arith.equal_nat Arith.equal_nat)
          (kvs_insert nat_ord
            (Arith.nat_of_integer (Big_int.big_int_of_int 6),
              Arith.nat_of_integer (Big_int.big_int_of_int 6))
            (List.map (fun x -> (x, x))
              [Arith.zero_nat; Arith.one_nat;
                Arith.nat_of_integer (Big_int.big_int_of_int 3);
                Arith.nat_of_integer (Big_int.big_int_of_int 4)]))
          (List.map (fun x -> (x, x))
            [Arith.zero_nat; Arith.one_nat;
              Arith.nat_of_integer (Big_int.big_int_of_int 3);
              Arith.nat_of_integer (Big_int.big_int_of_int 4);
              Arith.nat_of_integer (Big_int.big_int_of_int 6)]))
      in
    ();;

end;; (*struct Key_value*)

module Searching_and_splitting : sig
  type ('a, 'b, 'c) rsplit_node_ext =
    Rsplit_node_ext of 'a list * 'b list * 'b * 'a list * 'b list * 'c
  val r_ts2 : ('a, 'b, 'c) rsplit_node_ext -> 'b list
  val r_ts1 : ('a, 'b, 'c) rsplit_node_ext -> 'b list
  val r_ks2 : ('a, 'b, 'c) rsplit_node_ext -> 'a list
  val r_ks1 : ('a, 'b, 'c) rsplit_node_ext -> 'a list
  val r_t : ('a, 'b, 'c) rsplit_node_ext -> 'b
  val unsplit_node : ('a, 'b, unit) rsplit_node_ext -> 'a list * 'b list
  val mk_rsplit_node :
    ('a -> 'a -> Arith.int) ->
      'a -> 'a list * 'b list -> ('a, 'b, unit) rsplit_node_ext
  val rsplit_node_map :
    ('a -> 'b) ->
      ('c, 'a, unit) rsplit_node_ext -> ('c, 'b, unit) rsplit_node_ext
  val dest_rsplit_node :
    ('a, 'b, unit) rsplit_node_ext ->
      'a list * ('b list * ('b * ('a list * 'b list)))
  val rsplit_get_bounds :
    ('a, 'b, unit) rsplit_node_ext -> 'a option * 'a option
  val r_t_update :
    ('a -> 'a) -> ('b, 'a, 'c) rsplit_node_ext -> ('b, 'a, 'c) rsplit_node_ext
  val r_ks1_update :
    ('a list -> 'a list) ->
      ('a, 'b, 'c) rsplit_node_ext -> ('a, 'b, 'c) rsplit_node_ext
  val r_ks2_update :
    ('a list -> 'a list) ->
      ('a, 'b, 'c) rsplit_node_ext -> ('a, 'b, 'c) rsplit_node_ext
  val r_ts1_update :
    ('a list -> 'a list) ->
      ('b, 'a, 'c) rsplit_node_ext -> ('b, 'a, 'c) rsplit_node_ext
  val r_ts2_update :
    ('a list -> 'a list) ->
      ('b, 'a, 'c) rsplit_node_ext -> ('b, 'a, 'c) rsplit_node_ext
end = struct

type ('a, 'b, 'c) rsplit_node_ext =
  Rsplit_node_ext of 'a list * 'b list * 'b * 'a list * 'b list * 'c;;

let rec aux
  cmp k0 ks_rs1 ks_rs2 =
    let (ks1, rs1) = ks_rs1 in
    let (ks, rs) = ks_rs2 in
    let (r, rsa) = (List.hd rs, List.tl rs) in
    (match ks with [] -> ((ks1, rs1), (r, (ks, rsa)))
      | k :: ksa ->
        (if Key_value.key_lt cmp k0 k then ((ks1, rs1), (r, (ks, rsa)))
          else aux cmp k0 (k :: ks1, r :: rs1) (ksa, rsa)));;

let rec r_ts2
  (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) = r_ts2;;

let rec r_ts1
  (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) = r_ts1;;

let rec r_ks2
  (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) = r_ks2;;

let rec r_ks1
  (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) = r_ks1;;

let rec r_t (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) = r_t;;

let rec unsplit_node
  r = let ks = List.rev (Util.rev_apply r r_ks1) @ Util.rev_apply r r_ks2 in
      let a =
        List.rev (Util.rev_apply r r_ts1) @
          [Util.rev_apply r r_t] @ Util.rev_apply r r_ts2
        in
      (ks, a);;

let rec mk_rsplit_node
  cmp k ks_rs =
    let a = aux cmp k ([], []) ks_rs in
    let (aa, b) = a in
    let (ks1, rs1) = aa in
    (fun (r, (ks2, rs2)) -> Rsplit_node_ext (ks1, rs1, r, ks2, rs2, ()))
      b;;

let rec rsplit_node_map
  g f = Rsplit_node_ext
          (Util.rev_apply f r_ks1,
            Util.rev_apply (Util.rev_apply f r_ts1) (List.map g),
            Util.rev_apply (Util.rev_apply f r_t) g, Util.rev_apply f r_ks2,
            Util.rev_apply (Util.rev_apply f r_ts2) (List.map g), ());;

let rec dest_rsplit_node
  r = (Util.rev_apply r r_ks1,
        (Util.rev_apply r r_ts1,
          (Util.rev_apply r r_t,
            (Util.rev_apply r r_ks2, Util.rev_apply r r_ts2))));;

let rec rsplit_get_bounds
  rn = let l =
         (match Util.rev_apply rn r_ks1 with [] -> None | x :: _ -> Some x) in
       let a =
         (match Util.rev_apply rn r_ks2 with [] -> None | x :: _ -> Some x) in
       (l, a);;

let rec r_t_update
  r_ta (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) =
    Rsplit_node_ext (r_ks1, r_ts1, r_ta r_t, r_ks2, r_ts2, more);;

let rec r_ks1_update
  r_ks1a (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) =
    Rsplit_node_ext (r_ks1a r_ks1, r_ts1, r_t, r_ks2, r_ts2, more);;

let rec r_ks2_update
  r_ks2a (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) =
    Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2a r_ks2, r_ts2, more);;

let rec r_ts1_update
  r_ts1a (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) =
    Rsplit_node_ext (r_ks1, r_ts1a r_ts1, r_t, r_ks2, r_ts2, more);;

let rec r_ts2_update
  r_ts2a (Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2, more)) =
    Rsplit_node_ext (r_ks1, r_ts1, r_t, r_ks2, r_ts2a r_ts2, more);;

end;; (*struct Searching_and_splitting*)

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

module Tree : sig
  type ('a, 'b) tree = Node of ('a list * ('a, 'b) tree list) |
    Leaf of ('a * 'b) list
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
  Leaf of ('a * 'b) list;;

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
           not (List.null cs) &&
             List.pred_list
               (fun c ->
                 Arith.equal_nata (height c)
                   (height (List.nth cs Arith.zero_nat)))
               cs
         | Leaf _ -> true);;

let rec balanced t = Util.assert_true (forall_subtrees balanced_1 t);;

let rec wf_ks_rs_1
  t0 = (match t0
         with Node (l, cs) ->
           Arith.equal_nata (Arith.plus_nat Arith.one_nat (List.size_list l))
             (List.size_list cs)
         | Leaf _ -> true);;

let rec wf_ks_rs t0 = Util.assert_true (forall_subtrees wf_ks_rs_1 t0);;

let rec dest_Node
  = function Node (ks, rs) -> (ks, rs)
    | Leaf uu -> Util.failwitha "dest_Node";;

let rec tree_equal t1 t2 = Util.failwitha "FIXME patch";;

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
      (if Arith.equal_nata i min_child_index then None
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
  val rstack_map :
    ('a -> 'b) ->
      ('c, 'a, unit) Searching_and_splitting.rsplit_node_ext list ->
        ('c, 'b, unit) Searching_and_splitting.rsplit_node_ext list
  val no_focus :
    ('a, 'b, unit) Searching_and_splitting.rsplit_node_ext list ->
      ('a, ('b option), unit) Searching_and_splitting.rsplit_node_ext list
  val r_stk_to_rs :
    ('a, 'b, unit) Searching_and_splitting.rsplit_node_ext list -> 'b list
  val rstack_equal :
    ('a, 'b, unit) Searching_and_splitting.rsplit_node_ext list ->
      ('a, 'b, unit) Searching_and_splitting.rsplit_node_ext list -> bool
  val tree_to_rstack :
    ('a -> 'a -> Arith.int) ->
      'a -> ('a, 'b) Tree.tree ->
              Arith.nat ->
                ('a, 'b) Tree.tree *
                  ('a, ('a, 'b) Tree.tree, unit)
                    Searching_and_splitting.rsplit_node_ext list
  val rstack_get_bounds :
    ('a, 'b, unit) Searching_and_splitting.rsplit_node_ext list ->
      'a option * 'a option
  val add_new_stack_frame :
    ('a -> 'a -> Arith.int) ->
      'a -> 'a list * 'b list ->
              ('a, 'b, unit) Searching_and_splitting.rsplit_node_ext list ->
                ('a, 'b, unit) Searching_and_splitting.rsplit_node_ext list * 'b
end = struct

let rec rstack_map
  f stk =
    Util.rev_apply stk (List.map (Searching_and_splitting.rsplit_node_map f));;

let rec no_focus
  stk = Util.rev_apply (Util.rev_apply stk (rstack_map (fun a -> Some a)))
          (fun a ->
            (match a with [] -> []
              | frm :: aa ->
                Searching_and_splitting.r_t_update (fun _ -> None) frm :: aa));;

let rec r_stk_to_rs
  xs = Util.rev_apply xs (List.map Searching_and_splitting.r_t);;

let rec rstack_equal s1 s2 = Util.failwitha "FIXME patch";;

let rec tree_to_rstack
  ord k t n =
    (if Arith.equal_nata n Arith.zero_nat then (t, [])
      else (match tree_to_rstack ord k t (Arith.minus_nat n Arith.one_nat)
             with (Tree.Node (ks, ts), stk) ->
               let frm = Searching_and_splitting.mk_rsplit_node ord k (ks, ts)
                 in
               (Util.rev_apply frm Searching_and_splitting.r_t, frm :: stk)
             | (Tree.Leaf _, _) -> Util.failwitha "tree_to_stack"));;

let rec rstack_get_bounds
  = function [] -> (None, None)
    | x :: stk ->
        let (l, u) = Searching_and_splitting.rsplit_get_bounds x in
        let a =
          (match (l, u)
            with (None, _) ->
              let (la, ua) = rstack_get_bounds stk in
              ((if Option.is_none l then la else l),
                (if Option.is_none u then ua else u))
            | (Some _, None) ->
              let (la, ua) = rstack_get_bounds stk in
              ((if Option.is_none l then la else l),
                (if Option.is_none u then ua else u))
            | (Some la, Some ua) -> (Some la, Some ua))
          in
        let (aa, b) = a in
        (aa, b);;

let rec add_new_stack_frame
  cmp k ks_rs stk =
    let (ks, rs) = ks_rs in
    let r = Searching_and_splitting.mk_rsplit_node cmp k (ks, rs) in
    (r :: stk, Util.rev_apply r Searching_and_splitting.r_t);;

end;; (*struct Tree_stack*)

module Disk_node : sig
  type ('a, 'b, 'c) dnode = Disk_node of ('a list * 'c list) |
    Disk_leaf of ('a * 'b) list
  val mk_Disk_node : 'a list * 'b list -> ('a, 'c, 'b) dnode
  val dest_Disk_leaf : ('a, 'b, 'c) dnode -> ('a * 'b) list
  val dest_Disk_node : ('a, 'b, 'c) dnode -> 'a list * 'c list
end = struct

type ('a, 'b, 'c) dnode = Disk_node of ('a list * 'c list) |
  Disk_leaf of ('a * 'b) list;;

let rec check_length_ks_rs
  ks_rs =
    let (ks, rs) = ks_rs in
    Arith.equal_nata (Arith.plus_nat Arith.one_nat (List.size_list ks))
      (List.size_list rs);;

let rec mk_Disk_node
  ks_rs =
    let _ = Util.check_true (fun _ -> check_length_ks_rs ks_rs) in
    Disk_node ks_rs;;

let rec dest_Disk_leaf
  f = (match f with Disk_node _ -> Util.failwitha "dest_Disk_leaf"
        | Disk_leaf x -> x);;

let rec dest_Disk_node
  f = (match f with Disk_node x -> x
        | Disk_leaf _ -> Util.failwitha "dest_Disk_node");;

end;; (*struct Disk_node*)

module Pre_params : sig
  val dummy : unit
  val mk_r2t :
    ('a -> 'b -> ('c, 'd, 'b) Disk_node.dnode option) ->
      Arith.nat -> 'a -> 'b -> ('c, 'd) Tree.tree option
end = struct

let dummy : unit = ();;

let rec mk_r2ta
  r2f n t r =
    (if Arith.equal_nata n Arith.zero_nat then None
      else (match r2f t r with None -> None
             | Some (Disk_node.Disk_node (ks, rs)) ->
               let ts =
                 List.map (mk_r2ta r2f (Arith.minus_nat n Arith.one_nat) t) rs
                 in
               (match List.filter Util.is_None ts
                 with [] ->
                   Some (Tree.Node
                          (ks, Util.rev_apply ts (List.map Util.dest_Some)))
                 | _ :: _ -> None)
             | Some (Disk_node.Disk_leaf kvs) -> Some (Tree.Leaf kvs)));;

let rec mk_r2t x = mk_r2ta x;;

end;; (*struct Pre_params*)

module Params : sig
  type ('a, 'b, 'c, 'd, 'e) store_ops_ext =
    Store_ops_ext of
      ('c -> 'd -> 'd * ('a, 'b, 'c) Disk_node.dnode Util.res) *
        (('a, 'b, 'c) Disk_node.dnode -> 'd -> 'd * 'c Util.res) *
        ('c list -> 'd -> 'd * unit Util.res) * 'e
  type ('a, 'b, 'c, 'd) ps1 =
    Ps1 of
      (unit Prelude.constants_ext *
        (('a -> 'a -> Arith.int) * ('a, 'b, 'c, 'd, unit) store_ops_ext))
  val dummy : unit
  val dot_cmp : ('a, 'b, 'c, 'd) ps1 -> 'a -> 'a -> Arith.int
  val dot_constants : ('a, 'b, 'c, 'd) ps1 -> unit Prelude.constants_ext
  val dot_store_ops :
    ('a, 'b, 'c, 'd) ps1 -> ('a, 'b, 'c, 'd, unit) store_ops_ext
  val store_free :
    ('a, 'b, 'c, 'd, 'e) store_ops_ext -> 'c list -> 'd -> 'd * unit Util.res
  val store_read :
    ('a, 'b, 'c, 'd, 'e) store_ops_ext ->
      'c -> 'd -> 'd * ('a, 'b, 'c) Disk_node.dnode Util.res
  val store_alloc :
    ('a, 'b, 'c, 'd, 'e) store_ops_ext ->
      ('a, 'b, 'c) Disk_node.dnode -> 'd -> 'd * 'c Util.res
end = struct

type ('a, 'b, 'c, 'd, 'e) store_ops_ext =
  Store_ops_ext of
    ('c -> 'd -> 'd * ('a, 'b, 'c) Disk_node.dnode Util.res) *
      (('a, 'b, 'c) Disk_node.dnode -> 'd -> 'd * 'c Util.res) *
      ('c list -> 'd -> 'd * unit Util.res) * 'e;;

type ('a, 'b, 'c, 'd) ps1 =
  Ps1 of
    (unit Prelude.constants_ext *
      (('a -> 'a -> Arith.int) * ('a, 'b, 'c, 'd, unit) store_ops_ext));;

let dummy : unit = Pre_params.dummy;;

let rec dest_ps1 ps1 = let Ps1 (x, (y, z)) = ps1 in
                       (x, (y, z));;

let rec dot_cmp
  ps1 = Util.rev_apply (Util.rev_apply ps1 dest_ps1) (fun (_, (y, _)) -> y);;

let rec dot_constants
  ps1 = Util.rev_apply (Util.rev_apply ps1 dest_ps1) (fun (x, (_, _)) -> x);;

let rec dot_store_ops
  ps1 = Util.rev_apply (Util.rev_apply ps1 dest_ps1) (fun (_, (_, z)) -> z);;

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
  type ('a, 'b, 'c) find_state
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
                          ('a, 'c, unit)
                            Searching_and_splitting.rsplit_node_ext list)))) option
  val wellformed_find_state :
    ('a -> 'a -> Arith.int) ->
      ('b -> 'c -> ('a, 'd) Tree.tree option) ->
        ('a, 'd) Tree.tree -> 'b -> ('a, 'd, 'c) find_state -> bool
end = struct

type ('a, 'b, 'c) find_state =
  F_down of
    ('c * ('a * ('c * ('a, 'c, unit)
                        Searching_and_splitting.rsplit_node_ext list)))
  | F_finished of
      ('c * ('a * ('c * (('a * 'b) list *
                          ('a, 'c, unit)
                            Searching_and_splitting.rsplit_node_ext list))));;

let rec find_step
  ps1 fs =
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
    (match fs
      with F_down (r0, (k, (r, stk))) ->
        Util.rev_apply (Util.rev_apply store_ops Params.store_read r)
          (Monad.fmap
            (fun a ->
              (match a
                with Disk_node.Disk_node (ks, rs) ->
                  let (stka, ra) =
                    Tree_stack.add_new_stack_frame
                      (Util.rev_apply ps1 Params.dot_cmp) k (ks, rs) stk
                    in
                  F_down (r0, (k, (ra, stka)))
                | Disk_node.Disk_leaf kvs ->
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
           Tree_stack.rstack_equal
             (Util.rev_apply tstk (Tree_stack.rstack_map (fun a -> Some a)))
             (Util.rev_apply rstk (Tree_stack.rstack_map (r2t s))))
         in
       (match fs
         with F_down (_, (k, (r, stk))) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_rstack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_focus r t_fo) &&
             Util.assert_true (check_stack stk t_stk)
         | F_finished (_, (k, (r, (_, stk)))) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_rstack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_focus r t_fo) &&
             Util.assert_true (check_stack stk t_stk)));;

end;; (*struct Find*)

module Pre_insert : sig
  val split_leaf :
    unit Prelude.constants_ext ->
      ('a * 'b) list -> ('a * 'b) list * ('a * ('a * 'b) list)
  val split_node :
    unit Prelude.constants_ext ->
      'a list * 'b list -> ('a list * 'b list) * ('a * ('a list * 'b list))
end = struct

let rec split_leaf
  c kvs =
    let _ =
      Util.check_true
        (fun _ ->
          Arith.less_eq_nat
            (Arith.plus_nat (Util.rev_apply c Prelude.max_leaf_size)
              Arith.one_nat)
            (List.size_list kvs))
      in
    let cut_point =
      Arith.minus_nat
        (Arith.plus_nat (Util.rev_apply c Prelude.max_leaf_size) Arith.one_nat)
        (Util.rev_apply c Prelude.min_leaf_size)
      in
    let _ =
      Util.check_true
        (fun _ -> Arith.less_eq_nat cut_point (List.size_list kvs))
      in
    let (l, r) = Util.split_at cut_point kvs in
    let _ =
      Util.check_true
        (fun _ ->
          Arith.less_eq_nat (Util.rev_apply c Prelude.min_leaf_size)
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
          Util.check_true
            (fun _ ->
              Arith.less_eq_nat (Util.rev_apply c Prelude.min_node_keys)
                (List.size_list ks2))
          in
        let (rs1, rs2) =
          Util.split_at (Arith.plus_nat cut_point Arith.one_nat) rs in
        ((ks1, rs1), (k, (ks2, rs2)));;

end;; (*struct Pre_insert*)

module Insert : sig
  type ('a, 'b, 'c) i12_t = I1 of 'c | I2 of ('c * ('a * 'c))
  type ('a, 'b, 'c) insert_state = I_down of (('a, 'b, 'c) Find.find_state * 'b)
    | I_up of
        (('a, 'b, 'c) i12_t *
          ('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list)
    | I_finished of 'c
  val insert_step :
    ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) insert_state -> 'd -> 'd * ('a, 'b, 'c) insert_state Util.res
  val dest_i_finished : ('a, 'b, 'c) insert_state -> 'c option
  val mk_insert_state : 'a -> 'b -> 'c -> ('a, 'b, 'c) insert_state
  val wellformed_insert_state :
    unit Prelude.constants_ext ->
      ('a -> 'a -> Arith.int) ->
        ('b -> 'c -> ('a, 'd) Tree.tree option) ->
          ('a, 'd) Tree.tree ->
            'b -> 'a -> 'd -> ('a, 'd, 'c) insert_state -> bool
end = struct

type ('a, 'b, 'c) i12_t = I1 of 'c | I2 of ('c * ('a * 'c));;

type ('a, 'b, 'c) insert_state = I_down of (('a, 'b, 'c) Find.find_state * 'b) |
  I_up of
    (('a, 'b, 'c) i12_t *
      ('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list)
  | I_finished of 'c;;

let rec wf_d
  k_ord r2t t0 s d =
    Util.assert_true
      (let (fs, _) = d in
       Find.wellformed_find_state k_ord r2t t0 s fs);;

let rec wf_f
  cs k_ord r2t t0 s k v r =
    Util.assert_true
      (match r2t s r with None -> false
        | Some t ->
          Tree.wellformed_tree cs (Some Prelude.Small_root_node_or_leaf) k_ord
            t &&
            Key_value.kvs_equal
              (Util.rev_apply (Util.rev_apply t0 Tree.tree_to_kvs)
                (Key_value.kvs_insert k_ord (k, v)))
              (Util.rev_apply t Tree.tree_to_kvs));;

let rec wf_u
  r2t k_ord t0 s k v u =
    Util.assert_true
      (let _ = Find.wf_store_tree r2t s in
       let check_stack =
         (fun rstk tstk ->
           Tree_stack.rstack_equal
             (Util.rev_apply
               (Util.rev_apply rstk (Tree_stack.rstack_map (r2t s)))
               Tree_stack.no_focus)
             (Util.rev_apply
               (Util.rev_apply tstk (Tree_stack.rstack_map (fun a -> Some a)))
               Tree_stack.no_focus))
         in
       (match u
         with (I1 r, stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_rstack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_stack stk t_stk) &&
             (match r2t s r with None -> Util.assert_true false
               | Some t ->
                 Util.assert_true
                   (Key_value.kvs_equal (Util.rev_apply t Tree.tree_to_kvs)
                     (Util.rev_apply (Util.rev_apply t_fo Tree.tree_to_kvs)
                       (Key_value.kvs_insert k_ord (k, v)))))
         | (I2 (r1, (ka, r2)), stk) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_rstack k_ord k t0 (List.size_list stk) in
           Util.assert_true (check_stack stk t_stk) &&
             (match (r2t s r1, r2t s r2) with (None, _) -> false
               | (Some _, None) -> false
               | (Some t1, Some t2) ->
                 let (l, ua) = Tree_stack.rstack_get_bounds t_stk in
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
      (Util.rev_apply ps1 Params.dot_constants,
        Util.rev_apply ps1 Params.dot_cmp)
      in
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
    (match u with (_, []) -> Util.impossible1 "insert, step_up"
      | (I1 r, p :: stk) ->
        let (ks, rs) =
          Searching_and_splitting.unsplit_node
            (Searching_and_splitting.r_t_update (fun _ -> r) p)
          in
        Util.rev_apply
          (Util.rev_apply (Disk_node.mk_Disk_node (ks, rs))
            (Util.rev_apply store_ops Params.store_alloc))
          (Monad.fmap (fun ra -> (I1 ra, stk)))
      | (I2 (r1, (k, r2)), p :: stk) ->
        let (ks2, rs2) =
          (Util.rev_apply p Searching_and_splitting.r_ks2,
            Util.rev_apply p Searching_and_splitting.r_ts2)
          in
        let (ks, rs) =
          Searching_and_splitting.unsplit_node
            (Searching_and_splitting.r_ts2_update (fun _ -> r2 :: rs2)
              (Searching_and_splitting.r_ks2_update (fun _ -> k :: ks2)
                (Searching_and_splitting.r_t_update (fun _ -> r1) p)))
          in
        (match
          Arith.less_eq_nat (List.size_list ks)
            (Util.rev_apply cs Prelude.max_node_keys)
          with true ->
            Util.rev_apply
              (Util.rev_apply (Disk_node.mk_Disk_node (ks, rs))
                (Util.rev_apply store_ops Params.store_alloc))
              (Monad.fmap (fun r -> (I1 r, stk)))
          | false ->
            let (ks_rs1, (ka, ks_rs2)) = Pre_insert.split_node cs (ks, rs) in
            Util.rev_apply
              (Util.rev_apply (Disk_node.mk_Disk_node ks_rs1)
                (Util.rev_apply store_ops Params.store_alloc))
              (Monad.bind
                (fun r1a ->
                  Util.rev_apply
                    (Util.rev_apply (Disk_node.mk_Disk_node ks_rs2)
                      (Util.rev_apply store_ops Params.store_alloc))
                    (Monad.fmap (fun r2a -> (I2 (r1a, (ka, r2a)), stk)))))));;

let rec step_down
  ps1 d =
    let (fs, v) = d in
    Util.rev_apply (Find.find_step ps1 fs) (Monad.fmap (fun da -> (da, v)));;

let rec step_bottom
  ps1 d =
    let (cs, k_ord) =
      (Util.rev_apply ps1 Params.dot_constants,
        Util.rev_apply ps1 Params.dot_cmp)
      in
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
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
                      (Util.rev_apply (Disk_node.Disk_leaf kvsa)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.fmap (fun a -> I1 a))
                  | false ->
                    let (kvs1, (ka, kvs2)) = Pre_insert.split_leaf cs kvsa in
                    Util.rev_apply
                      (Util.rev_apply (Disk_node.Disk_leaf kvs1)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.bind
                        (fun r1 ->
                          Util.rev_apply
                            (Util.rev_apply (Disk_node.Disk_leaf kvs2)
                              (Util.rev_apply store_ops Params.store_alloc))
                            (Monad.fmap (fun r2 -> I2 (r1, (ka, r2)))))))
                in
              Util.rev_apply fo (Monad.fmap (fun foa -> (foa, stk))))));;

let rec insert_step
  ps1 s =
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
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
              (Util.rev_apply (Disk_node.mk_Disk_node ([k], [r1; r2]))
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
  cs k_ord r2t t0 s k v is =
    Util.assert_true
      (match is with I_down a -> wf_d k_ord r2t t0 s a
        | I_up a -> wf_u r2t k_ord t0 s k v a
        | I_finished a -> wf_f cs k_ord r2t t0 s k v a);;

end;; (*struct Insert*)

module Delete2 : sig
  type ('a, 'b, 'c) del_t = D_small_leaf of ('a * 'b) list |
    D_small_node of ('a list * 'c list) | D_updated_subtree of 'c
  type ('a, 'b, 'c) delete_state = D_down of (('a, 'b, 'c) Find.find_state * 'c)
    | D_up of
        (('a, 'b, 'c) del_t *
          (('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list * 'c))
    | D_finished of 'c
  val delete_step :
    ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) delete_state -> 'd -> 'd * ('a, 'b, 'c) delete_state Util.res
  val dest_d_finished : ('a, 'b, 'c) delete_state -> 'c option
  val mk_delete_state : 'a -> 'b -> ('a, 'c, 'b) delete_state
  val wellformed_delete_state :
    unit Prelude.constants_ext ->
      ('a -> 'a -> Arith.int) ->
        ('b -> 'c -> ('a, 'd) Tree.tree option) ->
          ('a, 'd) Tree.tree -> 'b -> 'a -> ('a, 'd, 'c) delete_state -> bool
end = struct

type ('a, 'b, 'c) del_t = D_small_leaf of ('a * 'b) list |
  D_small_node of ('a list * 'c list) | D_updated_subtree of 'c;;

type ('a, 'b, 'c) delete_state = D_down of (('a, 'b, 'c) Find.find_state * 'c) |
  D_up of
    (('a, 'b, 'c) del_t *
      (('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list * 'c))
  | D_finished of 'c;;

let rec wf_d
  k_ord r2f t0 s d =
    Util.assert_true
      (let (fs, _) = d in
       Util.assert_true (Find.wellformed_find_state k_ord r2f t0 s fs));;

let rec wf_f
  constants k_ord r2t t0 s k r =
    Util.assert_true
      (let t = Util.rev_apply (r2t s r) Util.dest_Some in
       Util.assert_true
         (Tree.wellformed_tree constants (Some Prelude.Small_root_node_or_leaf)
           k_ord t) &&
         Util.assert_true
           (Key_value.kvs_equal
             (Util.rev_apply (Util.rev_apply t0 Tree.tree_to_kvs)
               (Key_value.kvs_delete k_ord k))
             (Util.rev_apply t Tree.tree_to_kvs)));;

let rec wf_u
  constants k_ord r2t t0 s k u =
    Util.assert_true
      (let (fo, stk) = u in
       let check_stack =
         (fun rstk tstk ->
           Tree_stack.rstack_equal
             (Util.rev_apply
               (Util.rev_apply rstk (Tree_stack.rstack_map (r2t s)))
               Tree_stack.no_focus)
             (Util.rev_apply
               (Util.rev_apply tstk (Tree_stack.rstack_map (fun a -> Some a)))
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
             Tree_stack.tree_to_rstack k_ord k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Prelude.Small_root_node_or_leaf
               | _ :: _ -> Some Prelude.Small_leaf)
             in
           Util.assert_true (check_stack stk t_stk) &&
             (Util.assert_true (check_wf ms (Tree.Leaf kvs)) &&
               Util.assert_true (check_focus t_fo kvs))
         | D_small_node (ks, rs) ->
           let (t_fo, t_stk) =
             Tree_stack.tree_to_rstack k_ord k t0 (List.size_list stk) in
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
             Tree_stack.tree_to_rstack k_ord k t0 (List.size_list stk) in
           let ms =
             (match stk with [] -> Some Prelude.Small_root_node_or_leaf
               | _ :: _ -> None)
             in
           let t = Util.rev_apply (Util.rev_apply r (r2t s)) Util.dest_Some in
           Util.assert_true (check_stack stk t_stk) &&
             (Util.assert_true (check_wf ms t) &&
               Util.assert_true
                 (check_focus t_fo (Util.rev_apply t Tree.tree_to_kvs)))));;

let rec node_steal_right
  store_ops p c1 c2 =
    let (ks1, rs1) = c1 in
    let (k2 :: rest, r2 :: resta) = c2 in
    let (k1 :: ks2, _ :: rs2) =
      (Util.rev_apply p Searching_and_splitting.r_ks2,
        Util.rev_apply p Searching_and_splitting.r_ts2)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (ks1 @ [k1], rs1 @ [r2]) Disk_node.mk_Disk_node)
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r3 ->
          Util.rev_apply
            (Util.rev_apply
              (Util.rev_apply (rest, resta) Disk_node.mk_Disk_node)
              (Util.rev_apply store_ops Params.store_alloc))
            (Monad.bind
              (fun r4 ->
                Util.rev_apply
                  (Util.rev_apply
                    (Util.rev_apply
                      (Util.rev_apply
                        (Searching_and_splitting.r_ts2_update
                          (fun _ -> r4 :: rs2)
                          (Searching_and_splitting.r_ks2_update
                            (fun _ -> k2 :: ks2)
                            (Searching_and_splitting.r_t_update (fun _ -> r3)
                              p)))
                        Searching_and_splitting.unsplit_node)
                      Disk_node.mk_Disk_node)
                    (Util.rev_apply store_ops Params.store_alloc))
                  (Monad.bind Monad.return)))));;

let rec node_merge_right
  cs store_ops p c1 c2 =
    let (ks1, rs1) = c1 in
    let (ks2, rs2) = c2 in
    let (k2 :: p_ks2, _ :: p_rs2) =
      (Util.rev_apply p Searching_and_splitting.r_ks2,
        Util.rev_apply p Searching_and_splitting.r_ts2)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (ks1 @ [k2] @ ks2, rs1 @ rs2) Disk_node.mk_Disk_node)
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r4 ->
          Util.rev_apply
            (Util.rev_apply
              (Searching_and_splitting.r_ts2_update (fun _ -> p_rs2)
                (Searching_and_splitting.r_ks2_update (fun _ -> p_ks2)
                  (Searching_and_splitting.r_t_update (fun _ -> r4) p)))
              Searching_and_splitting.unsplit_node)
            (fun (ks, rs) -> Monad.return (ks, rs))));;

let rec leaf_steal_right
  store_ops p c1 c2 =
    let k3 :: k4 :: kvs2 = c2 in
    let (_ :: ks2, _ :: p_rs2) =
      (Util.rev_apply p Searching_and_splitting.r_ks2,
        Util.rev_apply p Searching_and_splitting.r_ts2)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (c1 @ [k3]) (fun a -> Disk_node.Disk_leaf a))
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r1 ->
          Util.rev_apply
            (Util.rev_apply
              (Util.rev_apply (k4 :: kvs2) (fun a -> Disk_node.Disk_leaf a))
              (Util.rev_apply store_ops Params.store_alloc))
            (Monad.bind
              (fun r2 ->
                Util.rev_apply
                  (Util.rev_apply
                    (Util.rev_apply
                      (Util.rev_apply
                        (Searching_and_splitting.r_ts2_update
                          (fun _ -> r2 :: p_rs2)
                          (Searching_and_splitting.r_ks2_update
                            (fun _ -> Product_Type.fst k4 :: ks2)
                            (Searching_and_splitting.r_t_update (fun _ -> r1)
                              p)))
                        Searching_and_splitting.unsplit_node)
                      Disk_node.mk_Disk_node)
                    (Util.rev_apply store_ops Params.store_alloc))
                  (Monad.bind Monad.return)))));;

let rec leaf_merge_right
  cs store_ops p c1 c2 =
    let (_ :: ks2, _ :: p_rs2) =
      (Util.rev_apply p Searching_and_splitting.r_ks2,
        Util.rev_apply p Searching_and_splitting.r_ts2)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (c1 @ c2) (fun a -> Disk_node.Disk_leaf a))
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r1 ->
          Util.rev_apply
            (Util.rev_apply
              (Searching_and_splitting.r_ts2_update (fun _ -> p_rs2)
                (Searching_and_splitting.r_ks2_update (fun _ -> ks2)
                  (Searching_and_splitting.r_t_update (fun _ -> r1) p)))
              Searching_and_splitting.unsplit_node)
            (fun (ks, rs) -> Monad.return (ks, rs))));;

let rec node_steal_left
  store_ops p c1 c2 =
    let (k1 :: rest, r1 :: resta) =
      Util.rev_apply c1 (fun (x, y) -> (List.rev x, List.rev y)) in
    let (ks2, rs2) = c2 in
    let (k2 :: ks1, _ :: rs1) =
      (Util.rev_apply p Searching_and_splitting.r_ks1,
        Util.rev_apply p Searching_and_splitting.r_ts1)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (List.rev rest, List.rev resta) Disk_node.mk_Disk_node)
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r3 ->
          Util.rev_apply
            (Util.rev_apply
              (Util.rev_apply (k2 :: ks2, r1 :: rs2) Disk_node.mk_Disk_node)
              (Util.rev_apply store_ops Params.store_alloc))
            (Monad.bind
              (fun r4 ->
                Util.rev_apply
                  (Util.rev_apply
                    (Util.rev_apply
                      (Util.rev_apply
                        (Searching_and_splitting.r_t_update (fun _ -> r4)
                          (Searching_and_splitting.r_ts1_update
                            (fun _ -> r3 :: rs1)
                            (Searching_and_splitting.r_ks1_update
                              (fun _ -> k1 :: ks1) p)))
                        Searching_and_splitting.unsplit_node)
                      Disk_node.mk_Disk_node)
                    (Util.rev_apply store_ops Params.store_alloc))
                  (Monad.bind Monad.return)))));;

let rec node_merge_left
  cs store_ops p c1 c2 =
    let (ks1, rs1) = c1 in
    let (ks2, rs2) = c2 in
    let (k2 :: p_ks1, _ :: p_rs1) =
      (Util.rev_apply p Searching_and_splitting.r_ks1,
        Util.rev_apply p Searching_and_splitting.r_ts1)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (ks1 @ [k2] @ ks2, rs1 @ rs2) Disk_node.mk_Disk_node)
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r4 ->
          Util.rev_apply
            (Util.rev_apply
              (Searching_and_splitting.r_ts1_update (fun _ -> p_rs1)
                (Searching_and_splitting.r_ks1_update (fun _ -> p_ks1)
                  (Searching_and_splitting.r_t_update (fun _ -> r4) p)))
              Searching_and_splitting.unsplit_node)
            (fun (ks, rs) -> Monad.return (ks, rs))));;

let rec leaf_steal_left
  store_ops p c1 c2 =
    let k2 :: kvs1 = List.rev c1 in
    let (_ :: ks1, _ :: p_rs1) =
      (Util.rev_apply p Searching_and_splitting.r_ks1,
        Util.rev_apply p Searching_and_splitting.r_ts1)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (List.rev kvs1) (fun a -> Disk_node.Disk_leaf a))
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r1 ->
          Util.rev_apply
            (Util.rev_apply
              (Util.rev_apply (k2 :: c2) (fun a -> Disk_node.Disk_leaf a))
              (Util.rev_apply store_ops Params.store_alloc))
            (Monad.bind
              (fun r2 ->
                Util.rev_apply
                  (Util.rev_apply
                    (Util.rev_apply
                      (Util.rev_apply
                        (Searching_and_splitting.r_ts1_update
                          (fun _ -> r1 :: p_rs1)
                          (Searching_and_splitting.r_ks1_update
                            (fun _ -> Product_Type.fst k2 :: ks1)
                            (Searching_and_splitting.r_t_update (fun _ -> r2)
                              p)))
                        Searching_and_splitting.unsplit_node)
                      Disk_node.mk_Disk_node)
                    (Util.rev_apply store_ops Params.store_alloc))
                  (Monad.bind Monad.return)))));;

let rec leaf_merge_left
  cs store_ops p c1 c2 =
    let (_ :: ks1, _ :: p_rs1) =
      (Util.rev_apply p Searching_and_splitting.r_ks1,
        Util.rev_apply p Searching_and_splitting.r_ts1)
      in
    Util.rev_apply
      (Util.rev_apply
        (Util.rev_apply (c1 @ c2) (fun a -> Disk_node.Disk_leaf a))
        (Util.rev_apply store_ops Params.store_alloc))
      (Monad.bind
        (fun r1 ->
          Util.rev_apply
            (Util.rev_apply
              (Searching_and_splitting.r_ts1_update (fun _ -> p_rs1)
                (Searching_and_splitting.r_ks1_update (fun _ -> ks1)
                  (Searching_and_splitting.r_t_update (fun _ -> r1) p)))
              Searching_and_splitting.unsplit_node)
            (fun (ks, rs) -> Monad.return (ks, rs))));;

let rec post_merge
  cs store_ops krs =
    let (ks, rs) = krs in
    (match
      Arith.less_nat (List.size_list ks)
        (Util.rev_apply cs Prelude.min_node_keys)
      with true -> Monad.return (D_small_node (ks, rs))
      | false ->
        Util.rev_apply
          (Util.rev_apply (Util.rev_apply (ks, rs) Disk_node.mk_Disk_node)
            (Util.rev_apply store_ops Params.store_alloc))
          (Monad.bind (fun r -> Monad.return (D_updated_subtree r))));;

let rec step_up
  ps1 du =
    let (f, stk) = du in
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
    let (alloc, read) =
      (Util.rev_apply store_ops Params.store_alloc,
        Util.rev_apply store_ops Params.store_read)
      in
    let cs = Util.rev_apply ps1 Params.dot_constants in
    let post_mergea = post_merge cs store_ops in
    (match stk with [] -> Util.impossible1 "delete, step_up"
      | p :: stka ->
        Util.rev_apply
          (match f
            with D_small_leaf kvs ->
              (match
                Util.is_Nil (Util.rev_apply p Searching_and_splitting.r_ks2)
                with true ->
                  let ks1 = Util.rev_apply p Searching_and_splitting.r_ks1 in
                  let _ =
                    Util.check_true
                      (fun _ -> (match ks1 with [] -> false | _ :: _ -> true))
                    in
                  let r =
                    List.hd (Util.rev_apply p Searching_and_splitting.r_ts1) in
                  Util.rev_apply
                    (Util.rev_apply (Util.rev_apply r read)
                      (Monad.fmap Disk_node.dest_Disk_leaf))
                    (Monad.bind
                      (fun left_kvs ->
                        (match
                          Arith.equal_nata (List.size_list left_kvs)
                            (Util.rev_apply cs Prelude.min_leaf_size)
                          with true ->
                            Util.rev_apply
                              (leaf_merge_left cs store_ops p left_kvs kvs)
                              (Monad.bind post_mergea)
                          | false ->
                            Util.rev_apply
                              (leaf_steal_left store_ops p left_kvs kvs)
                              (Monad.fmap (fun a -> D_updated_subtree a)))))
                | false ->
                  let r =
                    List.hd (Util.rev_apply p Searching_and_splitting.r_ts2) in
                  Util.rev_apply
                    (Util.rev_apply (Util.rev_apply r read)
                      (Monad.fmap Disk_node.dest_Disk_leaf))
                    (Monad.bind
                      (fun right_kvs ->
                        (match
                          Arith.equal_nata (List.size_list right_kvs)
                            (Util.rev_apply cs Prelude.min_leaf_size)
                          with true ->
                            Util.rev_apply
                              (leaf_merge_right cs store_ops p kvs right_kvs)
                              (Monad.bind post_mergea)
                          | false ->
                            Util.rev_apply
                              (leaf_steal_right store_ops p kvs right_kvs)
                              (Monad.fmap (fun a -> D_updated_subtree a))))))
            | D_small_node (ks, rs) ->
              (match
                Util.is_Nil (Util.rev_apply p Searching_and_splitting.r_ks2)
                with true ->
                  let ks1 = Util.rev_apply p Searching_and_splitting.r_ks1 in
                  let _ =
                    Util.check_true
                      (fun _ -> (match ks1 with [] -> false | _ :: _ -> true))
                    in
                  let r =
                    List.hd (Util.rev_apply p Searching_and_splitting.r_ts1) in
                  Util.rev_apply
                    (Util.rev_apply (Util.rev_apply r read)
                      (Monad.fmap Disk_node.dest_Disk_node))
                    (Monad.bind
                      (fun (l_ks, l_rs) ->
                        (match
                          Arith.equal_nata (List.size_list l_ks)
                            (Util.rev_apply cs Prelude.min_node_keys)
                          with true ->
                            Util.rev_apply
                              (node_merge_left cs store_ops p (l_ks, l_rs)
                                (ks, rs))
                              (Monad.bind post_mergea)
                          | false ->
                            Util.rev_apply
                              (node_steal_left store_ops p (l_ks, l_rs)
                                (ks, rs))
                              (Monad.fmap (fun a -> D_updated_subtree a)))))
                | false ->
                  let r =
                    List.hd (Util.rev_apply p Searching_and_splitting.r_ts2) in
                  Util.rev_apply
                    (Util.rev_apply (Util.rev_apply r read)
                      (Monad.fmap Disk_node.dest_Disk_node))
                    (Monad.bind
                      (fun (r_ks, r_rs) ->
                        (match
                          Arith.equal_nata (List.size_list r_ks)
                            (Util.rev_apply cs Prelude.min_node_keys)
                          with true ->
                            Util.rev_apply
                              (node_merge_right cs store_ops p (ks, rs)
                                (r_ks, r_rs))
                              (Monad.bind post_mergea)
                          | false ->
                            Util.rev_apply
                              (node_steal_right store_ops p (ks, rs)
                                (r_ks, r_rs))
                              (Monad.fmap (fun a -> D_updated_subtree a))))))
            | D_updated_subtree r ->
              Util.rev_apply
                (Util.rev_apply
                  (Util.rev_apply
                    (Searching_and_splitting.unsplit_node
                      (Searching_and_splitting.r_t_update (fun _ -> r) p))
                    Disk_node.mk_Disk_node)
                  alloc)
                (Monad.fmap (fun a -> D_updated_subtree a)))
          (Monad.fmap (fun y -> (y, stka))));;

let rec delete_step
  ps1 s =
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
    let alloc = Util.rev_apply store_ops Params.store_alloc in
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
                        Key_value.key_eq (Util.rev_apply ps1 Params.dot_cmp) x
                          k)
                      (Util.rev_apply kvs (List.map Product_Type.fst))
                    with true ->
                      let kvsa =
                        Util.rev_apply kvs
                          (List.filter
                            (fun x ->
                              not (Key_value.key_eq
                                    (Util.rev_apply ps1 Params.dot_cmp)
                                    (Product_Type.fst x) k)))
                        in
                      (match
                        Arith.less_nat (List.size_list kvsa)
                          (Util.rev_apply
                            (Util.rev_apply ps1 Params.dot_constants)
                            Prelude.min_leaf_size)
                        with true ->
                          Monad.return (D_up (D_small_leaf kvsa, (stk, r0a)))
                        | false ->
                          Util.rev_apply
                            (Util.rev_apply (Disk_node.Disk_leaf kvsa) alloc)
                            (Monad.fmap
                              (fun r ->
                                D_up (D_updated_subtree r, (stk, r0a)))))
                    | false -> Monad.return (D_finished r0a)))))
      | D_up (f, (stk, r0)) ->
        (match Util.is_Nil stk
          with true ->
            (match f
              with D_small_leaf kvs ->
                Util.rev_apply (Util.rev_apply (Disk_node.Disk_leaf kvs) alloc)
                  (Monad.fmap (fun a -> D_finished a))
              | D_small_node (ks, rs) ->
                (match Arith.equal_nata (List.size_list ks) Arith.zero_nat
                  with true -> Monad.return (D_finished (List.hd rs))
                  | false ->
                    Util.rev_apply
                      (Util.rev_apply (Disk_node.mk_Disk_node (ks, rs)) alloc)
                      (Monad.fmap (fun a -> D_finished a)))
              | D_updated_subtree r -> Monad.return (D_finished r))
          | false ->
            Util.rev_apply (step_up ps1 (f, stk))
              (Monad.fmap (fun (fa, stka) -> D_up (fa, (stka, r0)))))
      | D_finished _ -> Monad.return s);;

let rec dest_d_finished
  x = (match x with D_down _ -> None | D_up _ -> None
        | D_finished a -> Some a);;

let rec mk_delete_state k r = D_down (Find.mk_find_state k r, r);;

let rec wellformed_delete_state
  constants k_ord r2t t0 s k ds =
    Util.assert_true
      (match ds with D_down a -> wf_d k_ord r2t t0 s a
        | D_up (fo, (stk, r)) ->
          wf_u constants k_ord r2t t0 s k (fo, stk) &&
            (match r2t s r with None -> false | Some t -> Tree.tree_equal t t0)
        | D_finished a -> wf_f constants k_ord r2t t0 s k a);;

end;; (*struct Delete2*)

module Insert_many : sig
  type ('a, 'b, 'c) fo = I1 of ('c * ('a * 'b) list) |
    I2 of (('c * ('a * 'c)) * ('a * 'b) list)
  type ('a, 'b, 'c) ist =
    I_down of (('a, 'b, 'c) Find.find_state * ('b * ('a * 'b) list)) |
    I_up of
      (('a, 'b, 'c) fo *
        ('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list)
    | I_finished of ('c * ('a * 'b) list)
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
  I_up of
    (('a, 'b, 'c) fo *
      ('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list)
  | I_finished of ('c * ('a * 'b) list);;

let rec step_up
  ps1 u =
    let (cs, _) =
      (Util.rev_apply ps1 Params.dot_constants,
        Util.rev_apply ps1 Params.dot_cmp)
      in
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
    (match u with (_, []) -> Util.impossible1 "insert, step_up"
      | (I1 (r, kvs0), x :: stk) ->
        let (ks, rs) =
          Searching_and_splitting.unsplit_node
            (Searching_and_splitting.r_t_update (fun _ -> r) x)
          in
        Util.rev_apply
          (Util.rev_apply (Disk_node.mk_Disk_node (ks, rs))
            (Util.rev_apply store_ops Params.store_alloc))
          (Monad.fmap (fun ra -> (I1 (ra, kvs0), stk)))
      | (I2 ((r1, (k, r2)), kvs0), x :: stk) ->
        let (ks2, rs2) =
          (Util.rev_apply x Searching_and_splitting.r_ks2,
            Util.rev_apply x Searching_and_splitting.r_ts2)
          in
        let (ks, rs) =
          Searching_and_splitting.unsplit_node
            (Searching_and_splitting.r_ts2_update (fun _ -> [r1; r2] @ rs2)
              (Searching_and_splitting.r_ks2_update (fun _ -> k :: ks2) x))
          in
        (match
          Arith.less_eq_nat (List.size_list ks)
            (Util.rev_apply cs Prelude.max_node_keys)
          with true ->
            Util.rev_apply
              (Util.rev_apply (Disk_node.mk_Disk_node (ks, rs))
                (Util.rev_apply store_ops Params.store_alloc))
              (Monad.fmap (fun r -> (I1 (r, kvs0), stk)))
          | false ->
            let (ks_rs1, (ka, ks_rs2)) = Pre_insert.split_node cs (ks, rs) in
            Util.rev_apply
              (Util.rev_apply (Disk_node.mk_Disk_node ks_rs1)
                (Util.rev_apply store_ops Params.store_alloc))
              (Monad.bind
                (fun r1a ->
                  Util.rev_apply
                    (Util.rev_apply (Disk_node.mk_Disk_node ks_rs2)
                      (Util.rev_apply store_ops Params.store_alloc))
                    (Monad.fmap
                      (fun r2a -> (I2 ((r1a, (ka, r2a)), kvs0), stk)))))));;

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
  cs k_ord u kv newa existing =
    let csa = cs in
    let step =
      (fun (acc, newb) ->
        (match
          Arith.less_eq_nat
            (Arith.times_nat (Arith.nat_of_integer (Big_int.big_int_of_int 2))
              (Util.rev_apply csa Prelude.max_leaf_size))
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
    let (cs, k_ord) =
      (Util.rev_apply ps1 Params.dot_constants,
        Util.rev_apply ps1 Params.dot_cmp)
      in
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
    let (fs, (v, kvs0)) = d in
    (match Find.dest_f_finished fs
      with None -> Util.impossible1 "insert, step_bottom"
      | Some (r0, (k, (_, (kvs, stk)))) ->
        Util.rev_apply
          (Util.rev_apply store_ops Params.store_free
            (r0 :: Tree_stack.r_stk_to_rs stk))
          (Monad.bind
            (fun _ ->
              let (_, u) = Tree_stack.rstack_get_bounds stk in
              let (kvsa, kvs0a) = kvs_insert_2 cs k_ord u (k, v) kvs0 kvs in
              let fo =
                (match
                  Arith.less_eq_nat (List.size_list kvsa)
                    (Util.rev_apply cs Prelude.max_leaf_size)
                  with true ->
                    Util.rev_apply
                      (Util.rev_apply (Disk_node.Disk_leaf kvsa)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.fmap (fun r -> I1 (r, kvs0a)))
                  | false ->
                    let (kvs1, (ka, kvs2)) = split_leaf cs kvsa in
                    Util.rev_apply
                      (Util.rev_apply (Disk_node.Disk_leaf kvs1)
                        (Util.rev_apply store_ops Params.store_alloc))
                      (Monad.bind
                        (fun r1 ->
                          Util.rev_apply
                            (Util.rev_apply (Disk_node.Disk_leaf kvs2)
                              (Util.rev_apply store_ops Params.store_alloc))
                            (Monad.fmap
                              (fun r2 -> I2 ((r1, (ka, r2)), kvs0a))))))
                in
              Util.rev_apply fo (Monad.fmap (fun foa -> (foa, stk))))));;

let rec insert_step
  ps1 s =
    let (_, _) =
      (Util.rev_apply ps1 Params.dot_constants,
        Util.rev_apply ps1 Params.dot_cmp)
      in
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
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
              (Util.rev_apply (Disk_node.mk_Disk_node ([k], [r1; r2]))
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
  LS_down of ('c * ('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list)
  | LS_leaf of
      (('a * 'b) list *
        ('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list)
  | LS_up of ('a, 'c, unit) Searching_and_splitting.rsplit_node_ext list;;

let rec step_up
  fs = let _ = Util.check_true (fun _ -> not (List.null fs)) in
       (match fs with [] -> Util.failwitha "impossible: Leaf_stream.step_up"
         | f :: fsa ->
           (match Util.rev_apply f Searching_and_splitting.dest_rsplit_node
             with (_, (_, (_, (_, [])))) -> LS_up fsa
             | (ks1, (rs1, (r, (ks2, ra :: rs)))) ->
               let fa =
                 Searching_and_splitting.Rsplit_node_ext
                   (List.hd ks2 :: ks1, r :: rs1, ra, List.tl ks2, rs, ())
                 in
               LS_down (ra, fa :: fsa)));;

let rec step_leaf r = let a = r in
                      let (_, aa) = a in
                      LS_up aa;;

let rec step_down
  ps1 rfs =
    let (r, fs) = rfs in
    let store_ops = Util.rev_apply ps1 Params.dot_store_ops in
    Util.rev_apply (Util.rev_apply store_ops Params.store_read r)
      (Monad.fmap
        (fun a ->
          (match a
            with Disk_node.Disk_node (ks, rs) ->
              let ra = List.hd rs in
              let rsa = List.tl rs in
              let frm =
                Searching_and_splitting.Rsplit_node_ext
                  ([], [], ra, ks, rsa, ())
                in
              LS_down (ra, frm :: fs)
            | Disk_node.Disk_leaf kvs -> LS_leaf (kvs, fs))));;

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
