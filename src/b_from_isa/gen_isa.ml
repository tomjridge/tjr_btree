module Fun : sig
  val id : 'a -> 'a
  val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
end = struct

let rec id x = (fun xa -> xa) x;;

let rec comp f g = (fun x -> f (g x));;

end;;

module HOL : sig
  type 'a equal = {equal : 'a -> 'a -> bool}
  val equal : 'a equal -> 'a -> 'a -> bool
  val eq : 'a equal -> 'a -> 'a -> bool
end = struct

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

let rec eq _A a b = equal _A a b;;

end;;

module Orderings : sig
  type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool}
  val less_eq : 'a ord -> 'a -> 'a -> bool
  val less : 'a ord -> 'a -> 'a -> bool
  type 'a preorder = {ord_preorder : 'a ord}
  type 'a order = {preorder_order : 'a preorder}
  type 'a linorder = {order_linorder : 'a order}
  val max : 'a ord -> 'a -> 'a -> 'a
end = struct

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

type 'a preorder = {ord_preorder : 'a ord};;

type 'a order = {preorder_order : 'a preorder};;

type 'a linorder = {order_linorder : 'a order};;

let rec max _A a b = (if less_eq _A a b then b else a);;

end;;

module Arith : sig
  type nat
  val less_eq_nat : nat -> nat -> bool
  val less_nat : nat -> nat -> bool
  val linorder_nat : nat Orderings.linorder
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

let preorder_nat =
  ({Orderings.ord_preorder = ord_nat} : nat Orderings.preorder);;

let order_nat =
  ({Orderings.preorder_order = preorder_nat} : nat Orderings.order);;

let linorder_nat =
  ({Orderings.order_linorder = order_nat} : nat Orderings.linorder);;

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

let rec equal_nat
  m n = Big_int.eq_big_int (integer_of_nat m) (integer_of_nat n);;

let rec minus_nat
  m n = Nat (Orderings.max ord_integer Big_int.zero_big_int
              (Big_int.sub_big_int (integer_of_nat m) (integer_of_nat n)));;

let rec times_nat
  m n = Nat (Big_int.mult_big_int (integer_of_nat m) (integer_of_nat n));;

end;;

module List : sig
  val equal_lista : 'a HOL.equal -> 'a list -> 'a list -> bool
  val equal_list : 'a HOL.equal -> ('a list) HOL.equal
  val nth : 'a list -> Arith.nat -> 'a
  val upt : Arith.nat -> Arith.nat -> Arith.nat list
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val drop : Arith.nat -> 'a list -> 'a list
  val find : ('a -> bool) -> 'a list -> 'a option
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val null : 'a list -> bool
  val last : 'a list -> 'a
  val take : Arith.nat -> 'a list -> 'a list
  val concat : ('a list) list -> 'a list
  val filter : ('a -> bool) -> 'a list -> 'a list
  val member : 'a HOL.equal -> 'a list -> 'a -> bool
  val insert : 'a HOL.equal -> 'a -> 'a list -> 'a list
  val butlast : 'a list -> 'a list
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val map : ('a -> 'b) -> 'a list -> 'b list
  val pred_list : ('a -> bool) -> 'a list -> bool
  val removeAll : 'a HOL.equal -> 'a -> 'a list -> 'a list
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

let rec fold
  f x1 s = match f, x1, s with f, x :: xs, s -> fold f xs (f x s)
    | f, [], s -> s;;

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

let rec member _A
  x0 y = match x0, y with [], y -> false
    | x :: xs, y -> HOL.eq _A x y || member _A xs y;;

let rec insert _A x xs = (if member _A xs x then xs else x :: xs);;

let rec butlast
  = function [] -> []
    | x :: xs -> (if null xs then [] else x :: butlast xs);;

let rec hd (x21 :: x22) = x21;;

let rec tl = function [] -> []
             | x21 :: x22 -> x22;;

let rec map
  f x1 = match f, x1 with f, [] -> []
    | f, x21 :: x22 -> f x21 :: map f x22;;

let rec pred_list
  p x1 = match p, x1 with p, [] -> true
    | p, x :: xs -> p x && pred_list p xs;;

let rec removeAll _A
  x xa1 = match x, xa1 with x, [] -> []
    | x, y :: xs ->
        (if HOL.eq _A x y then removeAll _A x xs else y :: removeAll _A x xs);;

let rec gen_length
  n x1 = match n, x1 with n, x :: xs -> gen_length (Arith.suc n) xs
    | n, [] -> n;;

let rec size_list x = gen_length Arith.zero_nat x;;

end;;

module Set : sig
  type 'a set = Set of 'a list | Coset of 'a list
  val ball : 'a set -> ('a -> bool) -> bool
  val insert : 'a HOL.equal -> 'a -> 'a set -> 'a set
  val bot_set : 'a set
end = struct

type 'a set = Set of 'a list | Coset of 'a list;;

let rec ball (Set xs) p = List.pred_list p xs;;

let rec insert _A
  x xa1 = match x, xa1 with x, Coset xs -> Coset (List.removeAll _A x xs)
    | x, Set xs -> Set (List.insert _A x xs);;

let bot_set : 'a set = Set [];;

end;;

module String : sig
  type nibble = Nibble0 | Nibble1 | Nibble2 | Nibble3 | Nibble4 | Nibble5 |
    Nibble6 | Nibble7 | Nibble8 | Nibble9 | NibbleA | NibbleB | NibbleC |
    NibbleD | NibbleE | NibbleF
end = struct

type nibble = Nibble0 | Nibble1 | Nibble2 | Nibble3 | Nibble4 | Nibble5 |
  Nibble6 | Nibble7 | Nibble8 | Nibble9 | NibbleA | NibbleB | NibbleC | NibbleD
  | NibbleE | NibbleF;;

end;;

module Product_Type : sig
  val equal_proda : 'a HOL.equal -> 'b HOL.equal -> 'a * 'b -> 'a * 'b -> bool
  val equal_prod : 'a HOL.equal -> 'b HOL.equal -> ('a * 'b) HOL.equal
  val equal_unit : unit HOL.equal
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
end = struct

let rec equal_proda _A _B
  (x1, x2) (y1, y2) = HOL.eq _A x1 y1 && HOL.eq _B x2 y2;;

let rec equal_prod _A _B =
  ({HOL.equal = equal_proda _A _B} : ('a * 'b) HOL.equal);;

let rec equal_unita u v = true;;

let equal_unit = ({HOL.equal = equal_unita} : unit HOL.equal);;

let rec fst (x1, x2) = x1;;

let rec snd (x1, x2) = x2;;

end;;

module Lattices_Big : sig
  val max : 'a Orderings.linorder -> 'a Set.set -> 'a
end = struct

let rec max _A
  (Set.Set (x :: xs)) =
    List.fold
      (Orderings.max
        _A.Orderings.order_linorder.Orderings.preorder_order.Orderings.ord_preorder)
      xs x;;

end;;