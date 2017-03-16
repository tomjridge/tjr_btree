open Block_device

(* a store backed by a file ---------------------------------------- *)

(* we target Btree_api.Simple.STORE *)

module Filestore = struct
  open Btree_api

  include Btree_api.Simple

  type store = { 
    fd: Blkdev_on_fd.fd; 
    free_ref: page_ref;
  }  

  let lens = Lens.{from=(fun s -> (s.fd,s)); to_=(fun (fd,s) -> {s with fd=fd})}

  let lift x = x |> Sem.with_lens lens

  let page_size = Blkdev_on_fd.Block.sz

  open Blkdev_on_fd

  type 'a m = ('a,store) Sem.m

  let from_fd ~fd ~init = Unix.(
      let free_ref = (
        match init with
        | true -> 0
        | false -> (
            ignore (lseek fd 0 SEEK_SET);
            let len = lseek fd 0 SEEK_END in     
            assert (len mod page_size = 0);
            len / page_size))
      in
      {fd;free_ref})

  let return = Sem.return

  let inc_free: int -> unit m = (
    fun n s -> ({s with free_ref=s.free_ref+n},Ok ()))

  let get_free: unit -> int m = (fun () s -> (s,Ok s.free_ref))

  let set_free: int -> unit m = (
    fun n s -> ({s with free_ref=n},Ok ()))

  (* alloc without write; free block can then be used to write data
     outside the btree *)
  let alloc_block: unit -> page_ref m = (fun () ->
    get_free () |> bind (fun r -> inc_free 1 |> bind (fun () -> return r)))

  let alloc: page -> page_ref m = (
    fun p -> 
      alloc_block ()
      |> bind (
        fun r -> 
          Sem.with_lens lens (Blkdev_on_fd.write r p)
          |> bind (fun () -> return r)))

  let free: page_ref list -> unit m = (fun ps -> Sem.return ())

  let page_ref_to_page: page_ref -> page m = (
    fun r -> Blkdev_on_fd.read r |> lift)
  
  let dest_Store : store -> page_ref -> page = (
    fun s -> 
      let run = Sem.run_ref (ref s) in
      fun r -> run (page_ref_to_page r))

  let sync: unit -> unit m = (fun () -> 
      Blkdev_on_fd.sync () |> lift)

  let write: r -> blk -> unit m = (fun r blk ->
      Blkdev_on_fd.write r blk |> lift)

  let read: r -> blk m = (fun r ->
      Blkdev_on_fd.read r |> lift)

end

let _ = (module Filestore : Btree_api.STORE)

let _ = (module Filestore : Btree_api.Simple.STORE)




(* recycling filestore -------------------------------------------------------- *)

(* a filestore which caches page writes and recycles page refs *)

(* we maintain a set of blocks that have been allocated and not freed
   since last sync (ie which need to be written), and a set of page
   refs that have been allocated since last sync and freed without
   being synced (ie which don't need to go to store at all) *)

(* FIXME worth checking no alloc/free misuse? *)


module Set_r = Btree_util.Set_int


module Cache = Map.Make(
  struct 
    type t = Filestore.page_ref
    let compare: t -> t -> int = Pervasives.compare
  end)


module Recycling_filestore = struct
  open Btree_api

  type page_ref = Filestore.page_ref [@@deriving yojson]
  type page = Filestore.page
  let page_size = Filestore.page_size  

  type cache_t = page Cache.t
  type fns_t = Set_r.t
  type store = { 
    fs: Filestore.store; 
    cache: cache_t;  (* a cache of pages which need to be written *)
    freed_not_synced: fns_t  (* really this is "don't write to store on sync" *)
    (* could be a list - we don't free something that has already been freed *)
      (* FIXME don't we also need to know which were allocated since last sync? *)
  }

  type 'a m = ('a,store) Sem.m

  let lens = 
    Lens.({from=(fun s -> (s.fs,s)); to_=(fun (fs,s) -> {s with fs=fs})})

  let from_filestore = 
    fun fs -> {fs; cache=Cache.empty;freed_not_synced=Set_r.empty}

  let lift x = x |> Sem.with_lens lens

  let get_fns: unit -> fns_t m = (
    fun () s -> (s,Ok s.freed_not_synced))

  let get_1_fns: unit -> page_ref option m = Sem.(fun () -> 
      get_fns () |> bind 
        (fun fns -> 
           match (Set_r.is_empty fns) with
           | true -> return None
           | false -> 
             fns
             |> Set_r.min_elt 
             |> (fun r -> return (Some r))))

  let fns_remove: page_ref -> unit m = (
    fun r s -> 
      ({s with freed_not_synced=(Set_r.remove r s.freed_not_synced)},Ok()))

  let get_cache: unit -> cache_t m = (
    fun () s -> (s,Ok s.cache))

  let clear_cache: unit -> unit m = (
    fun () s -> ({s with cache=Cache.empty},Ok()))
  
  let cache_add: page_ref -> page -> unit m = (
    fun r p -> fun s -> ({s with cache=Cache.add r p s.cache},Ok ()))


  let free: page_ref list -> unit m = (
    fun ps -> 
    fun s -> 
      let s' = {
        s with
        freed_not_synced=(
          Set_r.union s.freed_not_synced (Set_r.of_list ps)) }
      in
      (s', Ok()))

  let alloc_block: unit -> page_ref m = 
    fun () -> Filestore.alloc_block () |> lift

  
  (* FIXME following should use the monad from filestore *)
  let alloc : page -> page_ref m = Sem.(
      fun p -> 
        get_1_fns () |> bind
          (fun r -> 
             match r with 
             | None -> (
                 (Filestore.alloc_block () |> lift) |> bind
                   (fun r ->            
                      cache_add r p |> bind
                        (fun () -> return r)))
             | Some r -> (
                 (* just return a ref we allocated previously *)
                 fns_remove r |> bind
                   (fun () -> 
                      cache_add r p |> bind
                        (fun () -> return r)))))

  let page_ref_to_page: page_ref -> page m = Sem.(
      fun r -> 
        get_cache() |> bind
          (fun cache -> 
             (* consult cache first *)
             (try Some(Cache.find r cache) with Not_found -> None) 
             |> (function
                 | Some p -> (return p)
                 | None -> (
                     (Filestore.page_ref_to_page r) |> lift))))

  let dest_Store : store -> page_ref -> page = (
    fun s r -> 
      try (Cache.find r s.cache) with Not_found -> Filestore.dest_Store s.fs r)

  (* FIXME on a sync, freed_not_synced needs to be updated? at least,
     it isn't quite right at the moment *)
  let rec sync: unit -> unit m = (fun () ->
      get_cache () |> Sem.bind
        (fun cache -> 
           let es = Cache.bindings cache in
           get_fns () |> Sem.bind
             (fun f_not_s -> 
                let rec loop es = (
                  match es with 
                  | [] -> (Sem.return ())
                  | (r,p)::es -> (
                      match (Set_r.mem r f_not_s) with 
                      | true -> loop es (* don't sync if freed *)
                      | false -> (
                          (Filestore.(write r p) |> lift)
                          |> Sem.bind (fun () -> loop es))))
                in
                loop es |> Sem.bind 
                  (fun () -> clear_cache ()) |> Sem.bind
                  (* make sure we sync these writes to disk *)
                  (fun () -> Filestore.(sync ()) |> lift)
             )))

  type r = page_ref
  type blk = Filestore.page

  let write: r -> blk -> unit m = (fun r blk ->
      Filestore.write r blk |> lift)

  let read: r -> blk m = (fun r ->
      Filestore.read r |> lift)

end


let _ = (module Recycling_filestore : Btree_api.STORE)

let _ = (module Recycling_filestore : Btree_api.Simple.STORE)




(* raw block device like /dev/sda1 ---------------------------------------- *)


(* reimpl of blkdev_on_fd *)

(*
module Raw_block_device = struct

  open Btree_api

  let block_size = Defaults.page_size

  module BLK_ = Blkdev_on_fd

  type blk = BLK_.blk

  (* 'a cc is a client request that expects a response of type 'a *)
  type _ cc =   (* client *)
    | Read : int -> blk cc
    | Write: int * blk -> unit cc
    | Sync: unit cc


  (* a sequence of computations returning 'b *)
  type 'b bind_t = 
    | Return: 'b -> 'b bind_t
    | Bind: ('a cc * ('a -> 'b bind_t)) -> 'b bind_t

  let bind f m = Bind(m,f)

  type t = {
    path: string;
    fd: Unix.file_descr;
    free: int (* free block *)
  }

  type 'a ss = ('a * t) (* server value of type 'a *)
      
  open Rresult

  (* lift client computation to server *)
  let rec lift: ('a -> 'b cc) -> ('a ss -> 'b cc ss) = (fun f ->
      fun (a,t) -> (f a, t))

  (* service a client request on the server *)
  let rec step: type g. g cc ss -> g ss = (fun (gm,t) ->
      match gm with
      | Read i -> (
          BLK_.read i |> Sem.run t.fd
          |> (fun (fd',Ok blk) -> (blk,t)))
      | Write (i,blk) -> (
          BLK_.write i blk |> Sem.run t.fd 
          |> (fun (fd',Ok ()) -> ((),t)))
      | Sync -> (
          ExtUnixSpecific.fsync t.fd;
          (),t)
    )

  (* evaluate a sequence of client requests using the server *)
  let rec eval: type b. b bind_t ss -> b ss = (fun (bnd,t) ->
      match bnd with
      | Return b -> (b,t)
      | Bind (am,a_bm) -> (
          step (am,t) 
          |> (fun (a,t') -> (a_bm a) |> (fun b -> eval (b,t')))))

end
*)



  (* FIXME really we should read the free ref from block 0
  let fd_to_nonempty_store: fd -> (store,string) result = (
    fun fd -> 
      let len = Unix.(lseek fd 0 SEEK_END) in     
      assert (len mod page_size = 0);
      let free_ref = len / page_size in
      {fd;free_ref})
*)

  (*
  let existing_file_to_new_store: string -> store = (fun s ->
      let fd = Blkdev_on_fd.open_file s in
      (* now need to write the initial frame *)
      let free_ref = 0 in
      {fd; free_ref})      
   *)

  (*
  let existing_file_to_new_store: string -> store = (fun fn ->
      Filestore.existing_file_to_new_store fn |> (fun fs ->
          {fs; cache=Cache.empty; freed_not_synced=Set_r.empty} ))
*)
    

