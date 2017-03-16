(* misc ---------------------------------------- *)

let dest_Some = function (Some x) -> x | _ -> failwith "dest_Some"

let option_map f = function Some x -> Some(f x) | _ -> None

let rec iter_step (f:'s -> 's option) (x:'s) = (
  let s' = f x in
  match s' with
  | None -> x
  | Some x' -> iter_step f x')


(* isa translations ---------------------------------------- *)

module X = struct

  let int_to_nat x = Gen_isa.(x |>Big_int.big_int_of_int|>Arith.nat_of_integer)
  let int_to_int x = Gen_isa.(
      x |>Big_int.big_int_of_int|>(fun x -> Arith.Int_of_integer x))

end



(* simplified structs ---------------------------------------- *)


module type KEY_VALUE_TYPES = Btree_api.KEY_VALUE

module type CONSTANTS = Btree_api.CONSTANTS

module type STORE = Btree_api.STORE


(* construct non-simple versions suitable for isa -------------------------- *)

module Mk_kv = functor (KV:KEY_VALUE_TYPES) -> struct
  open X
  include KV
  let key_eq k1 k2 = KV.key_ord k1 k2 = 0
  let key_ord k1 k2 = KV.key_ord k1 k2|>int_to_int
  let equal_keya k1 k2 = KV.key_ord k1 k2 = 0
  let equal_key = Gen_isa.{HOL.equal = equal_keya}
  let equal_value_ta = KV.equal_value
  let equal_value_t = Gen_isa.{HOL.equal = equal_value_ta}

end

module Mk_c = functor (C:CONSTANTS) -> struct
  type min_size_t = Small_root_node_or_leaf | Small_node | Small_leaf 
  let max_leaf_size = C.max_leaf_size |> X.int_to_nat
  let max_node_keys  = C.max_node_keys|>X.int_to_nat
  let min_leaf_size = C.min_leaf_size|>X.int_to_nat
  let min_node_keys = C.min_node_keys|>X.int_to_nat
end

module Mk_st = functor (ST:STORE) -> struct
  module ST = ST
  module T (* : Our.Store_t *) = struct
    type page = ST.page
    type store = ST.store
    type page_ref = ST.page_ref[@@deriving yojson]

    open Our
    open Btree_api
    let to_our_monad: 
      ('a,store) State_error_monad.m -> ('a,store) Our.Monad.m_t = (
      fun x -> Our.Monad.M(
          fun s -> (
              x |> State_error_monad.run s |> (fun r -> 
                  match r with
                  | (s',Ok(z)) -> (s',Our.Util.Ok(z))
                  | (s',Error e) -> 
                    (s',Our.Util.Error (Our.Util.String_error e)))
            )
        ))
    
    let free : page_ref list -> (unit, store) Monad.m_t = (fun ps ->
        ST.free ps |> to_our_monad)

    let alloc : page -> (page_ref, store) Monad.m_t = (fun p ->
        ST.alloc p |> to_our_monad)

    let dest_Store : store -> page_ref -> page = ST.dest_Store

    let page_ref_to_page : page_ref -> (page, store) Monad.m_t = (fun r ->
        ST.page_ref_to_page r |> to_our_monad)
  end

  let _ = (module T : Our.Store_t)

  include T
end


(* main functor ---------------------------------------- *)

(* this is the most general interface to the btree routines *)

module Main = struct
  module type S = sig
    module C : CONSTANTS
    module KV: KEY_VALUE_TYPES
    module ST: STORE
    (* this module's types depend on the previous modules *)
    module FT : sig
      open KV
      open ST
      type pframe =  
          Node_frame of (key list * page_ref list) |
          Leaf_frame of (key * value) list[@@deriving yojson]

      val frame_to_page : pframe -> page
      val page_to_frame : page -> pframe
    end
  end

  module Make = functor (S:S) -> (struct

    (* set up to call our.make ---------------------------------------- *)
    module S = S
    (* don't want these infecting the namespace *)
    module Btree_private = struct
      module C = Mk_c(S.C)
      module KV = Mk_kv(S.KV)
      module ST = Mk_st(S.ST)
      module Frame_types = struct
        module Store = ST
        module Key_value_types = struct 
          include KV
          type value_t = value
          let value_t_of_yojson = KV.value_of_yojson
          let value_t_to_yojson = KV.value_to_yojson
        end
        include S.FT
      end
    end

    (* use our.make functor ---------------------------------------- *)
    module Our_ = Our.Make(Btree_private.C)(Btree_private.Frame_types)


    (* Raw_map exposes a monad, but here we repeatedly run the step
       function; this allows us to check various invariants *)
    module Find_ = (struct  (* ---------------------------------------- *)
      open Our_
      module KV_ = Key_value_types
      
      (* FIXME wrap in constructor to get nice type? *)
      type t = {
        tree: Tree.tree;
        store: Store.store;
        fs: Find.find_state
      }

      let last_state : t option ref = ref None   

      let last_trans : (t*t) option ref = ref None

      let check_state s = (
        last_state:=Some(s);
        Test.log __LOC__;
        Test.log (s.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        Test.test (fun _ -> 
          assert (Find.wellformed_find_state s.store s.tree s.fs));            
      )

      let check_trans s s' = (
        last_trans:=Some(s,s');
        Test.log __LOC__;
        Test.log (s.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        Test.log (s'.tree |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        check_state s;
        check_state s'
      )

      let mk : KV_.key -> Store.page_ref -> Store.store -> t = 
        fun k0 r s -> {
            tree=Frame.r_to_t s r; store=s; fs=Find.mk_find_state k0 r}

      exception E of (t*string)

      let step : t -> t = (fun x ->
          x.store |> (Find.find_step x.fs |> Our.Monad.dest_M) 
          |> (fun (s',y) -> (s',Btree_util.rresult_to_result y))
          |> (fun (s',y) -> (
                match y with
                | Ok fs' -> ({ x with store = s';fs=fs'})
                | Error e -> raise (E({ x with store=s'}, e)))
            ))

      let dest s' = (s').fs|>Find.dest_f_finished

      (* loop until finished *)
      let find_1 : KV_.key -> Store.page_ref -> Store.store -> t = (
        fun k r st ->
          let s = ref (mk k r st) in
          let _ = check_state !s in
          let s' = s in
          let _ = 
            while(dest (!s') = None) do
              s := !s';
              s' := step !s;
              check_trans !s !s'
            done
          in
          (* !s' is None, so s holds the result *)
          !s'
      )

      open Btree_api

      let find: 
        KV_.key -> Store.page_ref -> (KV_.value_t option,Store.store) Sem.m = (
        fun k r st ->
          try (
            let s' = find_1 k r st in
            (* s' is finished *)
            let (r0,(k,(r,(kvs,stk)))) = s'|>dest|>dest_Some in
            let v = 
              try
                Some(
                  kvs|>List.find (function (x,y) -> Key_value.key_eq x k)
                  |>snd)
              with Not_found -> None
            in
            (s'.store,Ok v)
          ) with E(t,e) -> (t.store,Error e)
      )
    end)


    module Insert_ = (struct  (* ---------------------------------------- *)
      open Our_
      module KV_ = Key_value_types
      module ST_ = Store

      type t = {
        t: Tree.tree;
        k:KV_.key;
        v:KV_.value_t;
        store: Store.store;
        is: Insert.i_state_t
      }

      let last_state : t option ref = ref None   

      let last_trans : (t*t) option ref = ref None

      let check_state s = (
        last_state:=Some(s);
        Test.log __LOC__;
        Test.log ("s.t" ^ (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string));
        Test.log ("s.k" ^ (s.k |> KV_.key_to_yojson |> Yojson.Safe.to_string));
        Test.log ("s.v" ^ (s.v |> KV_.value_t_to_yojson |> Yojson.Safe.to_string));
        Test.log ("s.is" ^ (s.is |> Insert.i_state_t_to_yojson |> Yojson.Safe.to_string));
        Test.test (fun _ ->
            assert (Insert.wellformed_insert_state s.t s.k s.v s.store s.is))
      )
      
      let check_trans x y = (
        last_trans:=Some(x,y);
        (* Test.log __LOC__;
        Test.log (x.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        Test.log (y.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string); *)
        check_state x;
        check_state y
      )

      let mk: KV_.key -> KV_.value_t -> ST_.page_ref -> ST_.store -> t =
        fun k v r s -> { 
            t=(Frame.r_to_t s r);k;v;store=s;
            is=(Insert.mk_insert_state k v r)
          }

      exception E of (t*string)

      open Btree_util
      open Btree_api

      let step : t -> t = (fun x ->
          x.store |> (Insert.insert_step x.is |> Our.Monad.dest_M)
          |> (fun (s',y) -> (s',rresult_to_result y))
          |> (fun (s',y) -> 
              match y with
              | Ok is' -> ({ x with store = s'; is=is'})
              | Error e -> raise (E({ x with store=s'},e))))

      let dest s' = s'.is |> Insert.dest_i_finished

      let insert: KV_.key -> KV_.value_t -> ST_.page_ref -> (ST_.page_ref,ST_.store) Sem.m = (
        fun k v r store ->
          try (
            let s = ref (mk k v r store) in
            let _ = check_state !s in
            let s' = ref(!s) in
            let _ = 
              while((!s')|>dest = None) do
                s := !s';
                s' := step !s;
                check_trans !s !s';
              done
            in        
            let r = (!s')|>dest|>dest_Some in
            ((!s').store,Ok r)
          ) with E(t,e) -> (t.store,Error e))

    end)



    module Insert_many_ = struct  (* ---------------------------------------- *)

      open Our_
      module KV_ = Key_value_types
      module ST_ = Store

      type t = {
        t: Tree.tree;
        k:Key_value_types.key;
        v:Key_value_types.value_t;
        store: Store.store;
        is: Insert_many.i_state_t
      }

      let last_state : t option ref = ref None   

      let last_trans : (t*t) option ref = ref None

      let check_state s = (
        last_state:=Some(s);
        Test.log __LOC__;
        Test.log (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        Test.test (fun _ -> 
            assert (true))  (* FIXME *)
      )

      let check_trans x y = (
        last_trans:=Some(x,y);
        Test.log __LOC__;
        Test.log (x.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        Test.log (y.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        check_state x;
        check_state y
      )

      type kvs = (KV_.key * KV_.value_t) list

      let mk: KV_.key -> KV_.value_t -> kvs -> ST_.page_ref -> ST_.store -> t = 
        fun k v kvs r s -> {
            t=(Frame.r_to_t s r);k;v;store=s;
            is=(Insert_many.mk_insert_state k v kvs r)}

      exception E of (t*string)

      open Btree_util
      open Btree_api

      let step : t -> t = (fun x ->
          x.store |> (Insert_many.insert_step x.is|>Our.Monad.dest_M)
          |> (fun (s',y) -> (s',rresult_to_result y))
          |> (fun (s',y) ->
              match y with
              | Ok is' -> { x with store=s';is=is' }
              | Error e -> raise (E({ x with store=s'},e))))

      let dest s' = s'.is |> Insert_many.dest_i_finished

      let insert : KV_.key -> KV_.value_t -> kvs -> ST_.page_ref -> 
        (ST_.page_ref * kvs,ST_.store) Sem.m = (
        fun k v kvs r store ->
          try (
            let s = ref (mk k v kvs r store) in
            let _ = check_state !s in
            let s' = ref(!s) in
            let _ = 
              while((!s')|>dest = None) do
                s := !s';
                s' := step !s;
                check_trans !s !s';
              done
            in        
            let (r,kvs') = (!s')|>dest|>dest_Some in
            ((!s').store,Ok(r,kvs'))
          ) with E(t,e) -> (t.store,Error e))
    end


    module Delete_ = (struct  (* ---------------------------------------- *)
      open Our_
      module KV_ = Key_value_types
      module ST_ = Store

      type t = {
        t:Tree.tree;
        k:Key_value_types.key;
        store:Store.store;
        ds:Delete.d_state
      }

      let last_state : t option ref = ref None   

      let last_trans : (t*t) option ref = ref None

      let check_state s = (
        last_state:=Some(s);
        Test.log __LOC__;
        Test.log (s.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        Test.test (fun _ -> 
          assert (Delete.wellformed_delete_state s.t s.k s.store s.ds))
      )

      let check_trans x y = (
        last_trans:=Some(x,y);
        Test.log __LOC__;
        Test.log (x.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        Test.log (y.t |> Tree.tree_to_yojson |> Yojson.Safe.to_string);
        check_state x;
        check_state y
      )

      let mk : Store.store -> KV_.key -> Store.page_ref -> t = 
        fun s k r -> {
            t=(Frame.r_to_t s r);
            k;
            store=s;
            ds=(Delete.mk_delete_state k r)
          }

      exception E of (t*string)

      open Btree_util
      open Btree_api

      let step : t -> t = (fun x ->
          x.store |> (Delete.delete_step x.ds|>Our.Monad.dest_M)
          |> (fun (s',y) -> (s',rresult_to_result y))
          |> (fun (s',y) ->
              match y with
              | Ok ds' -> { x with store=s'; ds=ds' }
              | Error e -> raise (E({ x with store=s'},e))))

      let dest s' = s'.ds |> Delete.dest_d_finished

      let delete: KV_.key -> ST_.page_ref -> (ST_.page_ref,ST_.store) Sem.m = (
        fun k r store ->
          try (
            let s = ref (mk store k r) in
            let _ = check_state !s in
            let s' = ref(!s) in
            let _ = 
              while((!s')|>dest = None) do
                s := !s';
                s' := step !s;
                check_trans !s !s';
              done
            in
            let r = (!s')|>dest|>dest_Some in
            ((!s').store,Ok r)
          ) with E(t,e) -> (t.store,Error e))

      
      (* need some pretty *)
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

    end)



    (* Raw_map ---------------------------------------- *)

    module Raw_map (* : RAW_MAP *) = struct
      open Our_
      open Btree_api
      module KV = S.KV
      module ST = S.ST           let _ = (module ST: Btree_api.STORE)
      type bt_ptr = ST.page_ref
      type 'a m = ('a,ST.store * bt_ptr) Sem.m
      
      open KV

      let rresult_to_result = Btree_util.rresult_to_result

      let empty: unit -> (bt_ptr,ST.store) Sem.m = (
        fun () s -> (
            let m = Find.empty_btree ()|>Our.Monad.dest_M in
            m s |> (fun (s,r) -> (s,r|>rresult_to_result))
          ))

      let find: key -> value option m = (fun k ->
          fun (s,r) -> 
            Find_.find k r |> Sem.run s |> (fun (s',res) -> 
                ((s',r),res)))

      let insert: key -> value -> unit m = (fun k v ->
          fun (s,r) -> 
            Insert_.insert k v r |> Sem.run s |> (fun (s',res) ->
                match res with
                | Ok r' -> ((s',r'),Ok ())
                | Error e -> ((s',r),Error e)))

      let delete: key -> unit m = (
        fun k (s,r) -> 
          Delete_.delete k r |> Sem.run s |> (fun (s',res) ->
              match res with
                Ok r' -> ((s',r'),Ok ())
              | Error e -> ((s',r),Error e)))


      (* FIXME monad hassle *)
      let insert_many: key -> value -> (key*value) list -> unit m = (
        fun k v kvs -> Sem.(
            fun (s,r) -> 
              let rec loop r kvs = (
                match kvs with
                | [] -> return r
                | (k,v)::kvs -> (
                    Insert_many_.insert k v kvs r |> bind
                      (fun (r',kvs') -> 
                         loop r' kvs')))
              in
              loop r ((k,v)::kvs) |> Sem.run s |> (fun (s',res) -> 
                  match res with
                  | Ok r' -> ((s',r'),Ok ())
                  | Error e -> ((s',r),Error e))))


    end
    
    let _ = (module Raw_map : Btree_api.RAW_MAP)



    (* Leaf_stream_ ---------------------------------------- *)

    module Leaf_stream_ = (struct 
      open Btree_api
      open Btree_util
      open Our_
      module KV = Raw_map.KV
      module ST = Raw_map.ST
      open KV
      open Our_.Leaf_stream

      type t = Leaf_stream.ls_state
      type 'a m = ('a,ST.store * t) Sem.m

      let rresult_to_result = Btree_util.rresult_to_result

      (* repeatedly step till we get to the next leaf *)
      let step_till_leaf_or_finished: t -> (t option,ST.store) Sem.m = (
        let open Our.Monad in
        fun lss -> 
        fun s ->
          let rec loop lss = (
            lss_step lss|> bind 
              (fun lss' -> 
                 match lss_is_finished lss' with
                 | true -> (return None)
                 | false -> (
                     let kvs = dest_LS_leaf lss' in
                     match kvs with
                     | None -> loop lss'
                     | Some kvs -> return (Some lss'))))
          in
          loop lss |> Our.Monad.dest_M |> (fun run -> run s) 
          |> (fun (s',res) -> (s',res|>rresult_to_result)))

      let mk: ST.page_ref -> (t,ST.store) Sem.m = Sem.(
        fun r ->
          mk_ls_state r 
          |> step_till_leaf_or_finished 
          |> bind (
            fun topt ->
              match topt with 
              (* from an initial ref, should always get to leaf via
                 step_till_leaf_or_finished *)
              | None -> err ("impossible: " ^ __LOC__)
              | Some x -> return x))

      let step: unit -> bool m = (
        fun () ->
        fun (s,t) -> 
          t |> step_till_leaf_or_finished
          |> Sem.run s
          |> (fun (s',res) ->
              match res with 
              | Ok None -> ((s',t),Ok false)
              | Ok (Some t') -> ((s',t'),Ok true)
              | Error e -> ((s',t),Error e)))

      let get_kvs: unit -> (key*value) list m = (
        fun () ->
        fun (s,t) -> 
          t |> dest_LS_leaf 
          |> (fun kvs -> 
              match kvs with
              (* step_till_leaf_or_finished guarantees that we are
                 always at a state where dest_LS_leaf is Some *)
              | None -> ((s,t),Error (impossible __LOC__))
              | Some kvs -> ((s,t),Ok kvs))
      )

      (* for debugging *)
      let all_kvs: unit -> (key * value) list m = (
        fun () ->
          let rec loop kvs = (
            get_kvs () |> Sem.bind (fun kvs' -> 
                let kvs = kvs@kvs' in
                step () |> Sem.bind (fun b ->
                    if b then loop kvs else Sem.return kvs)))
          in
          loop [])


    end)  (* Leaf_stream_ *)

    let _ = (module Leaf_stream_ : Btree_api.LEAF_STREAM)

  end)  (* Make *)

end (* Main *)



    (* Imperative_leaf_stream ---------------------------------------- *)

    (*
    module Imperative_leaf_stream = (struct
      module ST = Raw_map.ST
      module KV = Raw_map.KV
      module LS = Leaf_stream_
      open KV          
      open Btree_api 
      let mk: ST.store -> ST.page_ref -> 
        (ST.store * LS.t) ref * (key,value) imperative_leaf_stream_t 
        = (
        fun s r ->
          let t = LS.mk r |> Sem.run s 
                  |> (fun (s',res) -> 
                      match res with 
                      | Ok t -> t
                      | Error e -> (failwith (__LOC__ ^ e)))
          in
          let st = ref (s,t) in
          let step () = (
            let (s,t) = (fst !st,snd !st) in
            LS.step t |> Sem.run s
            |> (fun (s',res) -> 
                st:=(s',t);
                match res with
                | Ok res -> (
                    match res with
                    | None -> false
                    | Some t' -> (st:=(s',t'); true)
                  )
                | Error e -> (failwith (__LOC__ ^ e))
              ))
          in
          let get_kvs () = (
            let (s,t) = (fst !st,snd !st) in
            LS.get_kvs t) 
          in
          (st,{ step; get_kvs }))

    end)

    let _ = (module Imperative_leaf_stream : Btree_api.IMPERATIVE_LEAF_STREAM)

*)



    (* Map_with_exceptions ---------------------------------------- *)

(*
    module Map_with_exceptions (* : MAP_WITH_EXCEPTIONS *) = struct
      module KV = Raw_map.KV
      module ST = Raw_map.ST
      type bt_ptr = ST.page_ref
                     
      open Btree_api
      type 'a m = ('a,ST.store * bt_ptr) State_monad.m

      let conv: 'a Raw_map.m -> 'a m = (fun m -> fun s ->
        m s |> (fun (s',res) -> (
              match res with
              | Ok x -> (s',x)
              | Error e -> failwith e)))

      open KV
      module RM_ = Raw_map

      let empty: ST.store -> ST.store * bt_ptr = (fun s ->
          RM_.empty |> Sem.run s |> (fun (s',res) ->
            match res with
            | Ok r' -> (s',r')
            | Error e -> failwith e))

      let insert: key -> value -> unit m = (fun k v ->
          RM_.insert k v|>conv)

      let insert_many: key -> value -> (key*value) list -> unit m = (
        fun k v kvs -> RM_.insert_many k v kvs |> conv)

      let find: key -> value option m = (fun k ->
          RM_.find k |> conv)

      let delete: key -> unit m = (fun k ->
          RM_.delete k |> conv)
    end
*)

    (* Imperative_map ---------------------------------------- *)

(*
    module Imperative_map = struct
      open Btree_api
      module T_ = (struct
        module ST = Raw_map.ST
        module KV = Raw_map.KV
        open KV
        module MWE_ = Map_with_exceptions
        let mk:ST.store ref -> ST.page_ref ref -> (key,value) imperative_map_t  
          = (fun s r -> 
              let lift = (fun f x ->  
                  (* lift f x = ... type check problems *)
                  f x |> S_m.run (!s,!r)
                  |>(fun ((s',r'),v) -> (
                        r:=r';
                        s:=s';
                        v
                      )))
              in
              let insert: key -> value -> unit = (fun k v ->
                  lift (fun (k,v) -> MWE_.insert k v) (k,v))
              in
              let insert_many: key -> value -> (key*value) list -> unit = (
                fun k v kvs -> 
                  lift (fun (k,v,kvs) -> MWE_.insert_many k v kvs) (k,v,kvs))
              in
              let find: key -> value option = (fun k ->
                  lift MWE_.find k)
              in
              let delete: key -> unit = (lift MWE_.delete) in
              { insert; insert_many; find; delete })

      end)  (* T_ *)

      let _ = (module T_ : IMPERATIVE_MAP)
      include T_

    end (* Imperative_map *)
*)
