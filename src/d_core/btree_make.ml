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
    val find: k -> page_ref -> store -> store * (page_ref*(k*v)list,string) result
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

          let dest: t -> finished option = fun s -> s.is |> Insert.dest_i_finished

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

      (* include ops in top level --------------------------------------------- *)

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


(* polymorphic, rather than modules ---------------------------------------- *)

module Poly = (struct

  type page_ref = int
  type ('k,'v) frame = ('k,'v,page_ref) Frame.t

  type ('a,'store) m = 'store -> ('store * ('a,string)result)

  type ('k,'v,'store) t = {
    compare_k: 'k -> 'k -> int;
    equal_v: 'v -> 'v -> bool;
    cs0: Isa_util.constants;
    store_free: page_ref list -> (unit,'store)  m;
    store_read : page_ref -> (('k, 'v) frame,'store) m;
    store_alloc : ('k, 'v) frame -> (page_ref,'store) m;
    mk_r2f: 'store -> page_ref -> ('k,'v) frame option;
  }


  type ('k,'v,'store) map = {
    find: 'k->page_ref->'store->'store*(page_ref * ('k*'v) list,string)result
  }

  module type Empty = sig end

  let make = (fun (type k') (type v') (type store') s ->
      let (module S : Isa_util.PARAMS with type k = k' and type v = v' and type store = store' and type page_ref = int) = (
        module struct
          type k = k'
          type v = v'
          let compare_k = s.compare_k
          let equal_v = s.equal_v
          type store = store'
          type 'a m = (store -> store * 'a Isa_util.res)
          type page_ref = int
          let cs0 = s.cs0
          let mk_r2f = s.mk_r2f 
          let store_read = s.store_read
          let store_alloc = s.store_alloc
          let store_free = s.store_free 
        end) 
      in
      (* let (module M : RR with module P = S) = (module Make_functor.Make(S)) in *)
      let (module M : Make_functor.RR with type P.k = k' and type P.v = v' and type P.store = store' and type P.page_ref = int) = (module Make_functor.Make(S)) in
      let r = { find=M.find } in
      r
    )

  

end)


(* poly, with the world monad ---------------------------------------- *)

module Poly_world = (struct

  type page_ref = int
  type ('k,'v) frame = ('k,'v,page_ref) Frame.t

  type ('a,'store) m = 'store -> ('store * ('a,string)result)

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

module Map = (struct

  open Poly_world

  (* FIXME use Poly_world.pre_map? remove Poly_world? *)
  type ('k,'v,'store) pre_wv = {
    r:page_ref;
    s:'store;
    find: 'k->page_ref->'store->'store*(page_ref * ('k*'v) list,string)result
  }

  type world_value = WV: ('k,'v,'store) pre_wv -> world_value

  type ('k,'v) map = world_value World.r

  (* this is an interface that mimics the standard map interface *)
  let find: ('k,'v) map -> 'k -> (page_ref * ('k*'v) list) World.m = (
    fun m k -> World.(
        get m |> bind (function WV wv -> (
              (* 'store existential in following *)
            let wv = ((Obj.magic wv) : ('k,'v,'store) pre_wv) in
            wv.find k wv.r wv.s |> (fun (s',res) -> 
                match res with
                | Ok (r,kvs) -> (
                    set m (WV {wv with s=s'; r=r}) 
                    |> bind (fun () -> return (r,kvs)))
                | Error e -> (
                    set m (WV {wv with s=s'})
                    |> bind (fun () -> Sem.err e)))))))

end)
