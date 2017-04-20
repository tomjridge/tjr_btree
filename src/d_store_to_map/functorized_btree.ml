(* construct map from store ---------------------------------------- *)

(* this includes code to construct initial states (eg init find_state)
   and apply the step functions repeatedly *)

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

(* don't use this directly - use the record version below *)
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
      open P

      module IU = Isa_util.Make(P)
      open IU

      (* FIXME avoid IEM. - add needed functions to eg IU.Find *)
      let mk_r2t r2f r = IU.IEM.Find.mk_r2t r2f (Isa_util.X.int_to_nat 1000) r
      let mk_r2t s r = mk_r2t (P.mk_r2f s) r


      (* find ---------------------------------------- *)

      (* Raw_map exposes a monad, but here we repeatedly run the step
         function; this allows us to check various invariants *)
      module Find_ = (struct 
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

          let dest: t -> finished option = fun s -> s.fs|>Find.dest_f_finished

          let step : t -> (t * (unit,string)result) = (fun x ->
              x.store 
              |> Find.find_step x.fs
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

        let find: k -> page_ref -> store -> 
          store * (page_ref*(k*v)list,string) result = (
          fun k r s ->
            let s = mk k r s in
            Iter_.run s |> (fun (s,r) -> (s.store,r)))
      end)


      (* insert ---------------------------------------- *)

      module Insert_ = (struct  
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
              |> Insert.insert_step x.is 
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
            fun s -> s.is |> Insert_many.dest_im_finished

          let step : t -> (t*(unit,string)result) = (fun x ->
              x.store 
              |> Insert_many.insert_many_step x.is
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
              |> Delete.delete_step x.ds
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


      (* leaf stream ---------------------------------------- *)

      module Leaf_stream_ = (struct 
        (* we need to repeatedly step the leaf state to the point that
           we hit a leaf and dest_LS_leaf <> None *)
        type ls_state = IU.Leaf_stream.ls_state
        let rec next_leaf: ls_state -> ls_state m = Simple_monad.(fun s ->
            s |> IU.Leaf_stream.lss_step |> bind (fun s' ->
                match (IU.Leaf_stream.dest_LS_leaf s') with
                | None -> next_leaf s'
                | Some _ -> return s'))
        let mk_leaf_stream : P.page_ref -> ls_state m = (fun r ->
          IU.Leaf_stream.mk_ls_state r |> next_leaf)
        let get_kvs: ls_state -> (k*v) list = (fun s ->
            s |> IU.Leaf_stream.dest_LS_leaf |> dest_Some)  (* guaranteed <> None *)

      end)



      (* FIXME remove this? *)
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






(* old ============================================================ *)


(*
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
 *)

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
