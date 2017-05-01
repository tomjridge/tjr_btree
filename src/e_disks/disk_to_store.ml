(* disk to store ---------------------------------------- *)

(* use pickling to convert a disk-like thing to a store-like thing *)

open Prelude
open Btree_api
open Page_ref_int
open Simple_monad
open Btree_with_pickle.O

type 't free_ops = {
  get_free: unit -> (int,'t) m;
  set_free: int -> (unit,'t) m;
}    


(* convert a disk to a store using pickling and a free counter; assume
   page size and block size are the same *)

let disk_to_store ps disk_ops free_ops : ('k,'v,'r,'t) store_ops = (
  let page_size = page_size ps in
  let pp = pp ps in
  assert (disk_ops.block_size = page_size);
  let store_free rs = (fun t -> (t,Ok())) in  (* no-op *)
  let store_alloc f : (page_ref,'t) m = 
    f|>frame_to_page page_size pp|> (fun p -> 
        free_ops.get_free () |> bind (fun free -> 
            disk_ops.write free p |> bind (fun () -> 
                free_ops.set_free (free+1) |> bind (fun () ->
                    return free))))
  in
  let store_read r : (('k,'v)frame,'t) m = 
    disk_ops.read r |> bind (fun blk ->
        page_to_frame page_size pp blk |> (fun frm ->
            return frm))
  in
  {store_free; store_read; store_alloc } 
)



(* FIXME where does this go?

  (* let cs0 = Constants.make_constants page_size tag_len pp.k_len pp.v_len in *)

  let r2f t r : ('k,'v) frame option = 
    store_read r t |> (function (_,Ok f) -> Some f 
                              | _ -> (ignore(assert(false)); None))
  in
*)

