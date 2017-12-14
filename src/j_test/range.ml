(* FIXME inefficient *)
let mk_range ~min ~max ~step = 
  let xs = ref [] in
  let n = ref min in
  while n <= max do
    xs:=!n::!xs;
    n:=!n+step
  done;
  List.rev !xs 
