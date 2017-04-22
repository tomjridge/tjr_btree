(* disk to store ---------------------------------------- *)

(* use pickling to convert a disk-like thing to a store-like thing *)

open Prelude
open Btree_api

module Make = functor (W:WORLD) -> (struct
    open W
    module Api = Make_api(W)
    open Api

    type ('k,'v) pp = ('k,'v) Pickle_params.t

    let tag_len = Btree_with_pickle.tag_len

    type free_ops = {
      get_free: unit -> int m;
      set_free: int -> unit m;
    }    

    module BWP = Btree_with_pickle

    (* convert a disk to a store using pickling and a free counter; assume
       page size and block size are the same; aim for Poly.t *)
    let disk_to_store page_size (disk_ops:disk_ops) (pp:('k,'v)Pickle_params.t) free_ops
      = (
        assert (disk_ops.block_size = page_size);
        let cs0 = Constants.make_constants page_size tag_len pp.k_len pp.v_len in
        let store_free: page_ref list -> unit m = (
          fun rs -> fun t -> (t,Ok()))  (* no-op *)
        in
        let store_alloc: ('k,'v) frame -> page_ref m = (fun f ->
            f|>BWP.frame_to_page page_size pp|> (fun p -> 
                free_ops.get_free () |> bind (fun free -> 
                    disk_ops.write free p |> bind (fun () -> 
                        free_ops.set_free (free+1) |> bind (fun () ->
                            return free)))))
        in
        let store_read: page_ref -> ('k,'v)frame m = (fun r ->
            disk_ops.read r |> bind (fun blk ->
                BWP.page_to_frame page_size pp blk |> (fun frm ->
                    return frm)))
        in
        let mk_r2f: t -> page_ref -> ('k,'v) frame option = (
          fun t r -> 
            store_read r t |> (function (_,Ok f) -> Some f 
                                      | _ -> (ignore(assert(false)); None)))
        in
        let r : ('k,'v)store_ops = {cs0; store_free; store_read; store_alloc; mk_r2f} in
        r
      )

  end)

