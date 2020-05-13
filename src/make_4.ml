(*

(** Like {!Make_3} but with {!Sh_std_ctxt}; don't open; has some examples *)


open Tjr_fs_shared.Sh_std_ctxt
open Make_3
open Btree_intf


class type ['k,'v] args = object
  method k_cmp: 'k -> 'k -> int
  (* method cs: Isa_btree.Constants.constants *)
  method k_mshlr: 'k bin_mshlr
  method v_mshlr: 'v bin_mshlr
end

(** NOTE hidden blk_id marshaller *)

(**/**)
module R_mshlr = struct
  type t = r[@@deriving bin_io]
  let max_sz = 9
end

let r_mshlr : blk_id bin_mshlr = (module R_mshlr)
(**/**)

  
(**/**)
let with_read_cache = true

let make ~(args:('k,'v)args) =
  let args:(_,_,_,_)Make_3.args = object
    method monad_ops=monad_ops
    method k_cmp=args#k_cmp
    method blk_sz=blk_sz
    (* method cs=args#cs *)
    method k_mshlr=args#k_mshlr
    method v_mshlr=args#v_mshlr
    method r_mshlr=r_mshlr
    (* method with_read_cache=true *)
  end
  in
  Make_3.make_uncached ~args (* ~blk_dev_ops ~blk_alloc ~root_ops *) |> fun (x,`K1 k) ->
  object
    method empty_leaf_as_blk=x.empty_leaf_as_blk
    method rest = fun ~(blk_dev_ops:std_blk_dev_ops) ~blk_alloc ~root_ops -> 
    k ~with_read_cache ~blk_dev_ops ~blk_alloc ~root_ops
  end
(**/**)

let make : 
args:('k, 'v) args ->
< empty_leaf_as_blk: unit -> ba_buf;
  rest: 
    blk_dev_ops:std_blk_dev_ops ->
    blk_alloc:(blk_id, t) blk_allocator_ops ->
    root_ops:(blk_id, t) with_state ->
    ('k, 'v, t) uncached_btree
>
= make
(** {[
args:('k, 'v) args ->
< empty_leaf_as_blk: unit -> ba_buf;
  rest: 
    blk_dev_ops:std_blk_dev_ops ->
    blk_alloc:(blk_id, t) blk_allocator_ops ->
    root_ops:(blk_id, t) with_state ->
    ('k, 'v, t) uncached_btree
>
]} *)

(** {2 Some examples} *)

(** The examples: int,int map; int,blk_id map; str_256,int map *)

let int_mshlr : int bin_mshlr = 
  let module A = struct
    open Bin_prot.Std
    type t = int[@@deriving bin_io]
    let max_sz=9
  end
  in
  (module A)

let bid_mshlr : blk_id bin_mshlr =
  let module A = struct
    (* open Bin_prot.Std *)
    type t = blk_id[@@deriving bin_io]
    let max_sz=9
  end
  in
  (module A)

let s256_mshlr : str_256 bin_mshlr = 
  let module A = struct
    (* open Bin_prot.Std *)
    open Str_256
    type t = str_256[@@deriving bin_io]
    let max_sz=259 (* FIXME check *)
  end
  in
  (module A)

(** NOTE hidden include of Sh_std_ctxt, to abbrev types in following *)
(**/**)
include Sh_std_ctxt
(**/**)

type ('k,'v) f = 
< empty_leaf_as_blk: unit -> ba_buf;
  rest: 
    blk_dev_ops:std_blk_dev_ops ->
    blk_alloc:(blk_id, t) blk_allocator_ops ->
    root_ops:(blk_id, t) with_state ->
    ('k, 'v, t) uncached_btree
>
(** This defn just to abbreviate types in the following

{[
type ('k,'v) f = 
< empty_leaf_as_blk: unit -> ba_buf;
  rest: 
    blk_dev_ops:std_blk_dev_ops ->
    blk_alloc:(blk_id, t) blk_allocator_ops ->
    root_ops:(blk_id, t) with_state ->
    ('k, 'v, t) uncached_btree
>
]}

 *)

let examples = 
  let open (struct
    let int_int_btree : (_,_)f = make ~args:(object 
        method k_cmp : int->int->int = Stdlib.compare
        method k_mshlr = int_mshlr
        method v_mshlr = int_mshlr
      end)

    let int_bid_btree : (_,_)f = make ~args:(object 
        method k_cmp : int->int->int = Stdlib.compare
        method k_mshlr = int_mshlr
        method v_mshlr = bid_mshlr
      end)

    let str_int_btree : (_,_)f = make ~args:(object
        method k_cmp: str_256 -> str_256 -> int = Stdlib.compare
        method k_mshlr=s256_mshlr
        method v_mshlr=int_mshlr
      end)

    let add_wbc = Store_write_back_cache.add_write_back_cache_to_store
                    ~monad_ops
  end)
  in
  object
    method uncached=object
      method int_int_btree=int_int_btree
      method int_bid_btree=int_bid_btree
      method str_int_btree=str_int_btree
    end
    method with_write_back_cache=object
      method int_bid_btree=
*)
