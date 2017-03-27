module Lens = struct
  (* 'a splits as (b,c) *)
  type ('l,'s,'r) t = {  (* large, small, rest *)
    from: 'l -> ('s * 'r);
    to_: ('s * 'r -> 'l)
  }
  let app lens f x = x |> lens.from |> (fun (s,r) -> (f s,r)) |> lens.to_
  let comp lens1 lens2 = {
    from=(fun l -> 
        let (l1,l2) = lens1.from l in
        let (l11,l12) = lens2.from l1 in
        (l11,(l12,l2)));
    to_=(fun (l11,(l12,l2)) -> 
        lens2.to_ (l11,l12) |> (fun l1 -> lens1.to_(l1,l2)))
  }
        
end
