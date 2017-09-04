(* some utilities for workign with byte ranges *)

(*
Intro: have two or more arrays and want to combine them somehow. Call the arrays (maps) m1 m2. m1 is defined on d1, m2 on d2. d1 and d2 are typically ranges of natural numbers eg 0...n. 

There is an operation of combining: m1+m2, which prefers m2 (non-commutative).

There is an operation of restriction m1|d 

A map is typically some (subrange within) a byte array, coupled with a domain translation (subrange (i,j) within the array corresponds to (i',j') which is the domain of the map.

We seek an expression for the result of combining two maps m3=m1+m2. The expression should be a list of adjacent ranges and a corresponding map which those ranges should be drawn from. The ranges should be non-trivial so we can decide whether m1 contributes to the final result. We should also be able to determine whether the result covers some particular range.

We represent a map via a byte array, an offset and a length, paired with another offset that represents the translation from one range to another.

Full array
Array+offset+length ~ subarray
subarray+offset ~ map   (defined from offset to offset+length-1)

The offset for the map works as follows. Subarray[0] corresponds to array[offset]. Map[offset] corresponds to subarray[0]. Thus the map offset shifts the subarray (which covers range 0...length-1) to cover (offset....). Map[x] is calculated by subtracting offset from x, to get the index into the subarray (and the subarray may not be defined for this x of course).

The restriction operator is probably best phrased in terms of restricting the length of the map. This allows it to be implemented directly on the subarray. The translation operation happens directly on the offset. The merge operation must be carried out carefully because m2 may be a subrange of m1. So there are 3 intervals potentially in play. So we need an operation that takes m1 and m2, and splits m1 into one or two maps that cover the non-m2 part. 

We need an operation m1\m2 which either gives one map (maybe with non-continuous range) or gives two. The second alternative means that m1\m2 is no longer an expression giving a single map. So it is better to move to allow a map to have multiple ranges? Or alternatively accept that m1\m2 is not an expression in the calculus, but some meta-expression denoting one or two actual maps.

So m1 in this case is not a continuous range anymore, but a sequence of ranges (but all with the same map offset). Alternatively, we can work with a normalization operation on a sequence of maps [m1,m2,...] which are either processed forwards or in reverse, and which give a final normalized expression for the range involved. 

Since we know that the map offset is always fixed, perhaps it then makes sense to generlize the submap to include a sequence of offset+length. But this is too ugly! But the alternative, of allowing m1\m2 as a metaoperation is also unpalatable. But let's go with it since we have little choice.

so m1\m2 is calculated as follows:

let M1 be the map based on m1, with domain n1...; if no intersection, we are done.

If n1... and n2... intersect, and n2' >=n1', we have one map resulting, M1|_<n2 (note that here it is easier to perform the restriction operation on the Map domain using positions rather than lengths- but easy to support both). Otherwise we have two: M1|_<n2 and M1|>=n2'

Normalization consists of constructing a list of maps, which are all adjacent and which partition the range (from min to max of ranges of initial maps) and which are non-trivial.

*)


let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

(* The underlying objects are expected to be subarrays within a larger
   array, i.e., be represented by an array, offset and length. In
   addition, they subarray maps to some range in the "result"
   array. *)
module type A = sig

  type 'a range
  type 'a t = 'a range

  val start: 'a t -> int
  val end_: 'a t -> int
  val set_start: int -> 'a t -> 'a t
  val set_end: int -> 'a t -> 'a t

end


module Mk_byte_range = functor (A:A) -> struct

  module A = A
  open A


(*
Suppose we have two intervals with starts and ends s1, s2, e1, e2.

Then at most 4 relative positions are possible, and 6 possible cases:

First four columns give relative order (<=), e.g., case (A) deals with
s1<=e1<=s2<=e2

|    |    |    |    | Comment                        |
|----+----+----+----+--------------------------------|
| s1 | e1 | s2 | e2 | (A) i2 after i1                |
|    |    |    |    |                                |
| s1 | s2 | e2 | e1 | (B) i2 a subinterval within i1 |
| s1 | s2 | e1 | e2 | (C) i2 overlaps the end of i1  |
|    |    |    |    |                                |
| s2 | e2 | s1 | e1 | (D) i2 before i1               |
| s2 | s1 | e2 | e1 | (E) i2 overlaps start of i1    |
| s2 | s1 | e1 | s2 | (F) i1 a subinterval  of i2    |
|    |    |    |    |                                |

Note that there are also questions whether s1=s2 etc., and note that
e1 represents the first position not covered by i1.

*)


  let merge' r1 r2 = (
    let (s1,e1) = (start r1,end_ r1) in
    let (s2,e2) = (start r2,end_ r2) in
    (* we use <= in the following to ensure we cover all cases,
       including when things are equal ... which we typically treat
       with an extra normalization step *)
    match () with
    | _ when e1 <= s2 ->  [r1;r2]
    | _ when s1 <= s2 && e2 <= e1 ->  (
        (* (B) i2 a subinterval within i1 *)
        let r1' = r1 |> set_end s2 in
        let r1'' = r1 |> set_start e2 in
        [r1';r2;r1''])  (* FIXME may have to normalize *)
    | _ when s1 <= s2 && e2 > e1 ->  (
        (* (C) i2 overlaps the end of i1 *)
        let r1' = r1 |> set_end s2 in
        [r1';r2])
    | _ when e2 <= s1 -> (
        (* (D) i2 before i1 *)
        [r2;r1])
    | _ when s2 <= s1 && e2 > s1 && e2 <= e1 -> (
        (* (E) i2 overlaps start of i1 *)
        let r1' = r1 |> set_start e2 in
        [r2;r1'])
    | _ when s2 <= s1 && e1 <= e2 -> (
        (* (F) i1 a subinterval  of i2 *)        
        [r2])
  )

  let normalize rs = List.filter (fun r -> end_ r - start r > 0) rs

  let merge r1 r2 = merge' r1 r2 |> normalize

end




(* now give typical implementation using subarrays; the array from
   start maps to a range from start+shift; expect 'a = 'b array *)

type 'a range' = { arr: 'a; start:int; end_:int; shift:int }

module B : A with type 'a range = 'a range' = struct

  type 'a range = 'a range'
  type 'a t = 'a range

  let start r = r.start+r.shift
  let end_ r = r.end_+r.shift
                        
  (* the new start position is post-shifting *)
  let set_start i r = 
    let i = i - r.shift in
    {r with start=i}

  let set_end i r = 
    let i = i - r.shift in
    {r with end_=i}
  
end


module Byte_range' = Mk_byte_range(B)


module Test = functor (X: sig end) -> struct

open Byte_range'


let r1_init = {arr=[| 10;11;12;13;14;15;16;17;18;19 |]; start=0; end_=10; shift=10 }

let r2_init = {arr=[| 20;21;22;23;24;25;26;27;28;29 |]; start=0; end_=10; shift=20 }

let (r1,r2) = (r1_init,r2_init)

let _ = merge r1 r2


let r2 = {r2_init with start=2;end_=4; shift=15}

let _ = merge r1 r2


(* C *)
let r2 = {r2_init with start=2;end_=7; shift=15}

let _ = merge r1 r2


(* D *)
let r2 = {r2_init with start=2;end_=4; shift=5}

let _ = merge r1 r2


(* E *)
let r2 = {r2_init with start=2;end_=4; shift=7}

let _ = merge r1 r2

(* F *)
let r1 = {r1_init with start=1; end_=5; shift=10}
let r2 = {r2_init with start=0;end_=10; shift=10}

let _ = merge r1 r2


end
