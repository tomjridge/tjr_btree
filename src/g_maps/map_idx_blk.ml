(* a map from index to blk, implemented as a map from index to bid *)

(*

we want to store a map from idx -> blk using the btree; a blk won't
fit as a value, so instead we use an idx -> blk_id map, where blk_id
is a pointer to the blk on disk

*)

open Prelude

open Internal_api

type idx = Types.blk_id [@@deriving yojson]
type blk = Types.blk [@@deriving yojson]

module type STORE = Internal_api.Simple.STORE 
  with type page = blk and type page_ref=idx

(* STORE.page = string which is Types.blk assuming lengths ok *)

module Make = functor (ST:STORE) -> (struct

    module ST = ST
    module MII = Map_int_int.Make(ST)
    module RM_ = MII.Btree_simple_internal.Btree.Raw_map

    (* NB this is not MII.KV, since value=blk not int *)
    module KV = struct
      type key = idx [@@deriving yojson]
      type value = blk [@@deriving yojson]
      let key_ord = Int_key.key_ord
      let equal_value (v1:value) v2 = (v1 = v2)
    end
    let _ = (module KV : KEY_VALUE)

    (* implement RAW_MAP *)
    module RM = struct
      module KV=KV
      module ST=ST
      type bt_ptr = ST.page_ref
      type 'a m = ('a,ST.store * bt_ptr) Sem.m

      open KV
      let empty: unit -> (bt_ptr,ST.store) Sem.m = RM_.empty

      let lens = Lens.({
          from=(fun (s,p) -> (s,p));
          to_=fun (s,p) -> (s,p);
        })

      let lift : 'a ST.m -> 'a m = fun x -> Sem.with_lens lens x

      (* alloc, then write ref into map *)
      let insert: key -> value -> unit m = Sem.(fun i blk ->
          (lift (ST.alloc blk)) |> bind (fun i' ->
              RM_.insert i i'))

      let insert_many: key -> value -> (key*value) list -> unit m = Sem.(
          fun i blk rest ->
            (* have to allocate lots of blocks then insert all refs into map *)
            let rec loop done_ xs = (
              match xs with
              | [] -> return done_
              | (i,blk)::xs -> (
                  (lift (ST.alloc blk)) |> bind (fun i' ->
                      loop ((i,i')::done_) xs))
            )
            in
            loop [] ((i,blk)::rest) |> bind (fun ijs ->
                match ijs with
                | [] -> failwith "impossible"
                | ((i,i')::rest) -> (
                    (* now insert many ii mappings *)
                    RM_.insert_many i i' rest)))

      let find: key -> value option m = Sem.(fun k ->
          (RM_.find k) |> bind (fun v ->
              match v with
              | None -> return None
              | Some v -> (
                  (* get the actual block *)
                  lift (ST.page_ref_to_page v) |> bind
                    (fun x -> return (Some x)))))

      let delete: key -> unit m = Sem.(fun k ->
          (* just delete from map; FIXME may want to free blk at some point *)
          RM_.delete k
        )

    end

    let _ = (module RM : RAW_MAP)

end)
