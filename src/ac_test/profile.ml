(* profiling ---------------------------------------- *)

let dest_Some = function Some x -> x | _ -> (failwith "dest_Some")

let now () = Core.Time_stamp_counter.(now () |> to_int63 |> Core.Std.Int63.to_int |> dest_Some)

module P = struct  (* timing points *)

  let ab = 1
  let ac = 13
  let bc = 2
  let cd = 3
  let de = 4
  let ef = 5
  let fg = 6
  let gh = 7
  let hi = 8
  let ij = 9
  let jk = 10

  let p_to_string i = (
      match i with
      | _ when i = ab -> "ab"
      | _ when i = ac -> "ac"
      | _ when i = bc -> "bc"
      | _ when i = cd -> "cd"
      | _ when i = de -> "de"
      | _ when i = ef -> "ef"
      | _ when i = fg -> "fg"
      | _ when i = gh -> "gh"
      | _ when i = hi -> "hi"
      | _ when i = ij -> "ij"
      | _ when i = jk -> "jk"
      | _ -> "FIXME"
    )              
                    
end

let ts = ref []

let log p = (ts := (p,now())::!ts; true)

let print_logs () = P.(
    let f last prev = (
        let (p2,t2) = last in
        let (p1,t1) = prev in
        let d = t2 - t1 in
        let s = Printf.sprintf "(%s,%s) %d" (p1|>p_to_string) (p2|>p_to_string) d in
        let _ = print_endline s in
        prev)
    in
    let _ = List.fold_left f (List.hd !ts) !ts in
    true)



let _ = Test.add_exit_hook (fun () -> assert(print_logs ()))
