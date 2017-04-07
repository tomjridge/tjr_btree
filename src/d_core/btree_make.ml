(* construct map from store ---------------------------------------- *)
open Prelude
(*open Btree_api *)


module Iter = struct
  module type S = sig
    type state
    type finished
    val check_state: state -> bool
    val check_trans: state -> state -> bool
    val step: state -> state * (unit,string)result
    val dest: state -> finished option
  end
  module Make = functor (S:S) -> (struct
      module S = S
      open S
      let rec run : state -> state*(finished,string)result = (fun s ->
        match dest s with
        | None -> (
            step s|> (fun x ->
                      match x with
                      | (s',Ok ()) -> (
                          assert(check_state s');
                          assert(check_trans s s');
                          run s')
                      | (s',Error e) -> (s,Error(e))))
        | Some x -> (s,Ok(x))
      )        
    end)
end


(* take params and produce isa_util.Make (done) *)

(* take params and produce proper find etc *)

module Make_functor = (struct 

  module type RR = sig
    module P : Isa_util.PARAMS
    open P
    val find: 
      k -> page_ref -> store -> store * (page_ref*(k*v)list,string) result
    val insert: k -> v -> page_ref -> store -> store * (page_ref,string)result
    val insert_many: k -> v -> (k*v)list -> 
      page_ref -> store-> store*(page_ref*(k*v)list,string)result
    val delete: k -> page_ref -> store -> store * (page_ref,string)result
  end

  module Make = functor (P:Isa_util.PARAMS) -> (struct

      module P_ = P

      module IU = Isa_util.Make(P)
      module IEM = IU.IEM

      open P

      let mk_r2t r2f r = IEM.Find.mk_r2t r2f (Isa_util.X.int_to_nat 1000) r
      let mk_r2t s r = mk_r2t (P.mk_r2f s) r


      (* find ---------------------------------------- *)

      (* Raw_map exposes a monad, but here we repeatedly run the step
         function; this allows us to check various invariants *)
      module Find_ = (struct 
        open IEM

        module S = (struct
          (* FIXME remove tree when not testing *)
          type t = {
            tree: (k,v) Tree.tree;
            store: P.store;
            fs: Find.find_state
          }

          type state = t

          let check_state s = (
            Test.log __LOC__;
            (* Test.log (s.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
            Test.test (fun _ -> 
                assert (Find.wellformed_find_state s.store s.tree s.fs));
            true
          )

          let check_trans s s' = (
            Test.log __LOC__;
            (*        Test.log (s.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
                      Test.log (s'.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
            (* check_state s && check_state s' - this shoudl check the transition not just the individual states *)
            true
          )

          type finished = page_ref * (k * v) list

          let dest: t -> finished option = fun s -> s.fs|>IU.dest_f_finished

          let step : t -> (t * (unit,string)result) = (fun x ->
              x.store 
              |> IU.find_step x.fs
              |> (fun (s',y) -> 
                  match y with
                  | Ok fs' -> ({ x with store=s';fs=fs'},Ok ())
                  | Error e -> ({x with store=s'},Error e)
                ))
        end)

        module Iter_ = Iter.Make(S)
        open S

        let mk : k -> page_ref -> store -> t = 
          fun k0 r s -> {
              tree=mk_r2t s r |> dest_Some; store=s; fs=Find.mk_find_state k0 r}

        let find: 
          k -> page_ref -> store -> store * (page_ref*(k*v)list,string) result = (
          fun k r s ->
            let s = mk k r s in
            Iter_.run s |> (fun (s,r) -> (s.store,r)))
      end)


      (* insert ---------------------------------------- *)

      module Insert_ = (struct  
        open IEM
        module S = (struct 
          type t = {
            t: (k,v) Tree.tree;
            k:k;
            v:v;
            store: P.store;
            is: Insert.insert_state
          }

          type state = t

          let check_state s = (
(*
          Test.log __LOC__;
          Test.log ("s.t" ^ (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string));
          Test.log ("s.k" ^ (s.k |> KV_.key_to_yojson |> Yojson.Safe.to_string));
          Test.log ("s.v" ^ (s.v |> KV_.value_t_to_yojson |> Yojson.Safe.to_string));
          Test.log ("s.is" ^ (s.is |> Insert.i_state_t_to_yojson |> Yojson.Safe.to_string));
*)
            Test.test (fun _ ->
                assert (Insert.wellformed_insert_state s.store s.t s.k s.v s.is));
            true
          )

          let check_trans x y = (true)

          type finished = page_ref

          let dest: t -> finished option = 
            fun s -> s.is |> Insert.dest_i_finished

          let step : t -> (t* (unit,string)result) = (fun x ->
              x.store 
              |> IU.insert_step x.is 
              |> (fun (s',y) -> 
                  match y with
                  | Ok is' -> ({ x with store=s';is=is'},Ok ())
                  | Error e -> ({x with store=s'},Error e)
                ))
        end)

        module Iter_ = Iter.Make(S)
        open S

        let mk: k -> v -> page_ref -> store -> t =
          fun k v r s -> 
            { t=mk_r2t s r|>dest_Some; 
              k; v; store=s; is=(Insert.mk_insert_state k v r) }

        let insert: k -> v -> page_ref -> store -> store * (page_ref,string)result = (
          fun k v r store ->
            let s = mk k v r store in
            Iter_.run s |> (fun (s,r) -> (s.store,r)))
      end)



      (* insert many ---------------------------------------- *)

      module Insert_many_ = (struct 
        open IEM
        module S = (struct
          type t = {
            t: (k,v) Tree.tree;
            k:k;
            v:v;
            store: P.store;
            is: Insert_many.i_state_t
          }
          type state = t

          let check_state s = (true) (* FIXME *)

          let check_trans x y = (true)

          type finished = (page_ref * (k*v) list) 

          let dest: state -> finished option = 
            fun s -> s.is |> Insert_many.dest_i_finished

          let step : t -> (t*(unit,string)result) = (fun x ->
              x.store 
              |> IU.insert_many_step x.is
              |> (fun (s',y) -> 
                  match y with
                  | Ok is' -> ({ x with store=s';is=is'},Ok ())
                  | Error e -> ({x with store=s'},Error e)
                ))

        end)
        module Iter_ = Iter.Make(S)
        open S

        type kvs = (k*v) list

        let mk: k -> v -> (k*v) list -> page_ref -> store -> t = 
          fun k v kvs r s -> {
              t=mk_r2t s r|>dest_Some;
              k; v; store=s; is=(Insert_many.mk_insert_state k v kvs r)}

        let insert_many: 
          k -> v -> kvs -> page_ref -> store-> store*(page_ref*kvs,string)result = (
          fun k v kvs r store ->
            let s = mk k v kvs r store in
            Iter_.run s |> (fun (s,r) -> (s.store,r)))
      end)


      (* delete ---------------------------------------- *)

      module Delete_ = (struct 
        open IEM
        module S = (struct           
          type t = {
            t:(k,v)Tree.tree;
            k:k;
            store:P.store;
            ds:Delete.delete_state
          }
          type state = t

          let check_state s = (
            Test.log __LOC__;
            (* Test.log (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
            Test.test (fun _ -> 
                assert (Delete.wellformed_delete_state s.t s.k s.store s.ds));
            true
          )

          let check_trans x y = (true)

          type finished = page_ref
          let dest: t -> finished option = fun s -> s.ds |> Delete.dest_d_finished

          let step : t -> (t*(unit,string)result) = (fun x ->
              x.store 
              |> IU.delete_step x.ds
              |> (fun (s',y) -> 
                  match y with
                  | Ok ds' -> ({ x with store=s';ds=ds'},Ok ())
                  | Error e -> ({x with store=s'},Error e)
                ))
        end)
        module Iter_ = Iter.Make(S)
        open S

        let mk: store -> k -> page_ref -> t = 
          fun s k r -> {
              t=mk_r2t s r |> dest_Some; (* FIXME obviously we don't want this testing in normal operation *)
              k;
              store=s;
              ds=(Delete.mk_delete_state k r)
            }

        let delete: k -> page_ref -> store -> store * (page_ref,string)result = (
          fun k r s ->
            mk s k r |> Iter_.run |> (fun (s,r) -> (s.store,r)))

        (* need some pretty *)
      (*
      let from_store s t = Delete.(
          let from_store s f = (
            match f with
              D_small_leaf kvs -> `D_small_leaf kvs
            | D_small_node(ks,rs) -> 
              `D_small_node(ks,rs|>List.map (Frame.r_to_t s))
            | D_updated_subtree(r) -> `D_updated_subtree(r|>Frame.r_to_t s)
          )
          in
          match t.ds with
          | D_down (fs,r) -> `D_down (* FIXME fs *)
          | D_up (f,(stk,r)) -> 
            `D_up(from_store s f,stk|>List.map (Frame.r_frame_to_t_frame s))
          | D_finished(r) -> `D_finished(r|>Frame.r_to_t s)
        )
     *)

      end)

      (* include ops in top level ------------------------------------------ *)

      module R = struct
        module P = P
        let find = Find_.find
        let insert = Insert_.insert 
        let delete = Delete_.delete
        let insert_many = Insert_many_.insert_many
      end

      let _ = (module R : RR)

      include R
    end) (* Make *)
end) (* Make_functor *)


(* remove functor ---------------------------------------- *)

(* we want to get versions which use records rather than functors *)


(* different ops address different maps etc, and store page_ref in different places *)

module Make_map_ops = functor (W:Btree_api.WORLD) -> (struct
    open Btree_api

    module BA = Btree_api.Make(W)

    open W
    open BA

    type page_ref_ops = {
      get_page_ref: W.t -> page_ref;
      set_page_ref: page_ref -> W.t -> W.t;
    }

    (* produce a ('k,'v) Map.ops, with page_ref state set/get via monad_ops *)
    let make: page_ref_ops -> ('k,'v) Store.ops -> ('k,'v) Map.ops = (
      fun (type k') (type v') ops s ->
      let 
        (module S: Isa_util.PARAMS 
          with type k = k' and type v = v' and  type store = W.t and type page_ref = int) 
        = 
        (module struct
          type k = k'
          type v = v'
          let compare_k = Store.(s.compare_k)
          let equal_v = s.equal_v
          type store = W.t
          type 'a m = store -> (store * ('a,string)result)
          type page_ref = int
          let cs0 = s.cs0
          let mk_r2f = s.mk_r2f 
          let store_read = s.store_read
          let store_alloc = s.store_alloc
          let store_free = s.store_free 
        end) 
      in
      (* let (module M : RR with module P = S) = (module Make_functor.Make(S)) in *)
      let 
        (module M : Make_functor.RR 
          with type P.k = k' and type P.v = v' and type P.store = W.t and type P.page_ref = int)
        = 
        (module Make_functor.Make(S))
      in
      let find: k' -> v' option m = fun k w ->
        let r = ops.get_page_ref w in
        M.find k r w |> (fun (w',res) -> 
            match res with
            | Ok (r',kvs) -> (w'|>ops.set_page_ref r', Ok (try Some(List.assoc k kvs) with _ -> None))
            | Error e -> (w', Error e))
      in
      let insert: (k' * v') -> unit m = fun (k,v) w ->
        let r = ops.get_page_ref w in
        M.insert k v r w |> (fun (w',res) -> 
            match res with
            | Ok (r') -> (w'|>ops.set_page_ref r', Ok ())
            | Error e -> (w', Error e))
      in
      let delete: k' -> unit m = fun k w ->
        let r = ops.get_page_ref w in
        M.delete k r w |> (fun (w',res) -> 
            match res with
            | Ok (r') -> (w'|>ops.set_page_ref r', Ok ())
            | Error e -> (w', Error e))
      in
      let get_leaf_stream: unit -> (k',v') LS.t m = fun () w ->
        let r = ops.get_page_ref w in
        failwith "FIXME"
      in
      Map.{find; insert; delete; get_leaf_stream})

  

end)


(* disk to store ---------------------------------------- *)

(* use btree_pickle to convert a disk-like thing to a store-like
   thing *)

module Disk_to_store = (struct
  module BA = Btree_api
  module BLK = BA.BLK
                 

  (* 't is the state of the underlying disk *)
  type 't disk_ops = 't BA.Disk.ops

  module Target_ = struct
    type ('k,'v,'store) t = ('k,'v,'store) Poly.t 
  end

  type ('k,'v) pp = ('k,'v) Btree_with_pickle.Pickle_params.t

  let tag_len = Btree_with_pickle.tag_len

  type 't store = {
    disk: 't;
    free: int
  }    

  type ('a,'t) m = ('a,'t) Poly.m
  type page_ref = Poly.page_ref

  type ('k,'v) frame = ('k,'v) Poly.frame
  
  module BWP = Btree_with_pickle

  (* convert a disk to a store using pickling and a free counter; assume
     page size and block size are the same; aim for Poly.t *)
  let disk2store page_size (disk_ops:'disk disk_ops) (pp:('k,'v) pp) 
      compare_k equal_v 
    = (
      assert (disk_ops.block_size = page_size);
      let cs0 = Constants.make_constants page_size tag_len pp.k_len pp.v_len in
      let store_free: page_ref list -> (unit,'disk store) m = (
        fun rs -> fun t -> (t,Ok()))  (* no-op *)
      in
      let store_alloc: ('k,'v) frame -> (page_ref,'disk store) m = (fun f ->
          f|>BWP.frame_to_page page_size pp|>(fun p ->
              fun (s:'disk store) -> 
                disk_ops.write s.free p s.disk 
                |> (fun (disk',res) -> (
                      match res with
                      | Ok () -> ({free=s.free+1; disk=disk'},Ok s.free)
                      | Error e -> ({s with disk=disk'},Error e)))))
      in
      let store_read: page_ref -> (('k,'v)frame,'disk store) m = (
        fun r (s:'disk store) ->
          disk_ops.read r (s.disk) |> (fun (disk',res) -> 
              match res with
              | Ok blk -> (
                  blk |> BWP.page_to_frame page_size pp |> (fun f -> 
                      {s with disk=disk'},Ok f))
              | Error e -> (s,Error e)))
      in
      let mk_r2f: 'disk store -> page_ref -> ('k,'v) frame option = (
          fun s r -> 
            store_read r s |> (function (_,Ok f) -> Some f 
                                      | _ -> (ignore(assert(false)); None)))
      in
      let r : ('k,'v,'disk store) Poly.t = 
        Poly.({compare_k; equal_v; cs0; 
               store_free; store_read; store_alloc; mk_r2f} )
      in
      r
    )

end)


(* FIXME do we need something that takes a disk (with World.m) and produces a map (with World.m)? or a Poly.store to a Poly.store with World.m? *)


(* if we have access to some state passing map, with arbitrary impl type 't *)
module Map_final = struct

  type ('k,'v,'t) pre_map = {
    find: 'k -> 't -> ('t * ('v option,string)result)
  }

  type ('k,'v) map = WV: ('k,'v,'t) pre_map -> ('k,'v) map

  module type Ops = sig
    val find: ('k,'v) map -> 'k -> 'v option World.m
  end

end



(* old ============================================================ *)


(* poly, with the world monad ---------------------------------------- *)

(*
module Poly_world = (struct

  type page_ref = int
  type ('k,'v) frame = ('k,'v,page_ref) Frame.t

  (* type ('a,'store) m = 'store -> ('store * ('a,string)result) *)
                                 
(*                                 
  type ('k,'v,'store) t = {
    compare_k: 'k -> 'k -> int;
    equal_v: 'v -> 'v -> bool;
    cs0: Isa_util.constants;
    store_free: page_ref list -> (unit,'store)  m;
    store_read : page_ref -> (('k, 'v) frame,'store) m;
    store_alloc : ('k, 'v) frame -> (page_ref,'store) m;
    mk_r2f: 'store -> page_ref -> ('k,'v) frame option;
  }
*)

  type 'store t = ('store*page_ref) World.r

  type ('k,'v,'store) pre_map = {
    find: 'store t -> 'k -> (page_ref * ('k*'v) list) World.m
  }

  (*
  type ('k,'v,'t) map' = {
    find: 't -> 'k -> (page_ref * ('k*'v) list) World.m
  }
  *)

  (* FIXME could use this type instead of exposing 'store above *)
  type ('k,'v) tt = T': ('k,'v,'store) pre_map * 'store * page_ref -> ('k,'v) tt

  
  let make = (fun (poly:('k,'v,'store)Poly.map) -> 
      let find: 'store t -> 'k -> (page_ref * ('k*'v) list) World.m = (
        fun wr k -> World.(
            get wr |> bind (fun (s,r) -> 
                poly.find k r s |> (fun (s',res) ->                    
                        match res with
                        | Ok (r,kvs) -> (
                            set wr (s',r) |> bind (fun () -> return (r,kvs)))
                        | Error e -> Sem.err e))))
      in
      {find})


end)
*)

(* map-like interface ---------------------------------------- *)

(*
module Map = (struct

  open Poly_world

  (* FIXME use Poly_world.pre_map? remove Poly_world? *)
  type ('k,'v,'store) pre_wv = {
    r:page_ref;
    s:'store;
    find: 'k->page_ref->'store->'store*(page_ref * ('k*'v) list,string)result
  }

  type ('k,'v) world_value = WV: ('k,'v,'store) pre_wv -> ('k,'v) world_value

  type ('k,'v) map = ('k,'v) world_value World.r

  (* this is an interface that mimics the standard map interface *)
  let find: ('k,'v) map -> 'k -> (page_ref * ('k*'v) list) World.m = (
    fun m k -> World.(
        get m |> bind (function WV wv -> (
              (* 'store existential in following *)
              (* let wv = ((Obj.magic wv) : ('k,'v,'store) pre_wv) in *)
              wv.find k wv.r wv.s |> (fun (s',res) -> 
                  match res with
                  | Ok (r,kvs) -> (
                      set m (WV {wv with s=s'; r=r}) 
                      |> bind (fun () -> return (r,kvs)))
                  | Error e -> (
                      set m (WV {wv with s=s'})
                      |> bind (fun () -> Sem.err e)))))))

end)
*)





(* if we have access to a map using a world *)
(*
module Map_final' = struct

  type ('k,'v,'wr) pre_map = {
    find: 'wr -> 'k -> 'v option World.m
  }

  type ('k,'v) wv = WV: ('k,'v,'t) pre_map -> ('k,'v) wv

  type ('k,'v) map = ('k,'v) wv World.r

  module type Ops = sig
    val find: ('k,'v) map -> 'k -> 'v option World.m
  end

end
*)

(* making ref type explicit *)

(*
module Map_final' = struct

  type ('k,'v,'r,'t) pre_map = {
    find: 'r -> 'k -> 't -> 't * ('v option,string) result
  }

  type ('k,'v) wv = WV: ('k,'v,'r,'t) pre_map -> ('k,'v) wv

  type ('k,'v) map = ('k,'v) wv World.r

  module type Ops = sig
    val find: ('k,'v) map -> 'k -> 't -> 't * ('v option,string)result
  end

end
*)
