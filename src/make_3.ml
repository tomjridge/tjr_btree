(** This is like make_2, but we try to use objects only, no functors
   and no type generation.  *)

(** FIXME need to remove cs from interface (also in make_2); need to
   add read cache; need to add version with wbc *)

(* open Btree_intf *)

(** Given the relevant sizes (blocks,keys,values), construct the
   B-tree size constants *)
let make_constants = Isa_btree.Constants.mk_constants

(* FIXME merge following types with those from examples, into btree_intf *)

(** B-tree, no write-back cache *)

type finished = {finished:bool}

class type ['k, 'v, 't ] ls = object
  method ls_step: unit -> (finished,'t)m
  method ls_kvs: unit -> ('k*'v)list
end

(** NOTE uncached here means "no write back cache"; a read cache is
   not observable except for performance and memory usage *)    
class type ['k, 'v, 't ] uncached_btree = 
  object
    method find        : 'k -> ('v option,'t)m
    method insert      : 'k -> 'v -> (unit,'t)m
    method delete      : 'k -> (unit,'t)m
    (* method insert_many : ('k*'v) -> ('k*'v)list -> (('k*'v)list,'t)m *)
    (* method insert_all  : ('k*'v)list -> (unit,'t)m *)
    method ls_create   : unit -> (('k,'v,'t)ls,'t)m
    (* method empty_leaf_as_blk: unit -> ba_buf *)
  end 

module type Bin_mshlr = sig
  type t[@@deriving bin_io]
  val max_sz: int
end

type 'a bin_mshlr = (module Bin_mshlr with type t='a)

class type ['k,'v,'r,'t] args = object
  method monad_ops: 't monad_ops
  method k_cmp: 'k -> 'k -> int
  method blk_sz: blk_sz
  (* method cs: Constants.constants *)
  method k_mshlr: 'k bin_mshlr
  method v_mshlr: 'v bin_mshlr
  method r_mshlr: 'r bin_mshlr
  (* method with_read_cache: bool *)
end

(* blk = buf = ba_buf *)

type empty_leaf_as_blk = { empty_leaf_as_blk: unit -> ba_buf }

let make_uncached (type k v r t)
      ~(args:(k,v,r,t)args)
  =
  let open (struct

    module S = struct
      type nonrec k=k
      type nonrec v=v
      type nonrec r=r
      type nonrec t=t
      type blk_id=r
      type blk=ba_buf
      type buf=blk
      let monad_ops = args#monad_ops
      let k_cmp = args#k_cmp
      let blk_sz = args#blk_sz
      (* let cs = args#cs *)
      let k_mshlr=args#k_mshlr
      let v_mshlr=args#v_mshlr
      let r_mshlr=args#r_mshlr
    end

    module M2 = Make_2.Make(S)
  end)
  in
  M2.{empty_leaf_as_blk}, `K1 begin fun 
    ~with_read_cache
    ~blk_dev_ops
    ~blk_alloc
    ~root_ops
    ->
      M2.make_uncached_btree ~with_read_cache
        ~blk_dev_ops ~blk_alloc ~root_ops |> fun bt ->
      let monad_ops = args#monad_ops in
      let ( >>= ) = monad_ops.bind in
      let return = monad_ops.return in
      let map_ops : (_,_,_,_,_)Btree_intf.map_ops_with_ls = bt#map_ops in
      let ls_create = fun () ->        
        bt#ls_create () >>= fun (ls:M2.ls) ->
        let ls = ref ls in
        let ls_step () = (bt#ls_step) !ls >>= fun (x:M2.ls option) -> 
          match x with
          | None -> return {finished=true}
          | Some x -> ls:=x; return {finished=false} 
        in
        let ls_kvs () = bt#ls_kvs !ls in
        let obj : (k,v,t) ls = object method ls_step=ls_step method ls_kvs=ls_kvs end in
        return obj
      in            
      (object 
        method find = fun k -> map_ops.find ~k
        method insert = fun k v -> map_ops.insert ~k ~v
        method delete = fun k -> map_ops.delete ~k
        method ls_create = ls_create
        (* method empty_leaf_as_blk=bt#empty_leaf_as_blk *)
      end : (k,v,t) uncached_btree)
  end
