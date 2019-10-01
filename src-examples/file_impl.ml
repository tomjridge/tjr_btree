(** A simple file implementation. Primarily this is here so that we
   can use it to marshall the free list 

Terminology: (file) block-index map: the map from blk index to blk_id

We implement a file as a map from blk_index to blk. The map is
typically implemented by a B-tree or similar.

The inode contains the file size and the root of the blk_index map.

New blocks are allocated as needed (by default the map is empty, and a
block is only allocated when that block is written to; so files are
"sparse").

The "iterated block read/write" component has been abstracted out
because it appears in a lot of other places.

The components we require are as follows (see "make" function below; NOTE missing free, since
this is not currently implemented for file.truncate).

{%html: 
<pre>
let make (type fid blk blk_id t) 
      ~monad_ops
      ~(blk_ops       : blk blk_ops)
      ~(blk_dev_ops   : (blk_id,blk,t) blk_dev_ops)
      ~(blk_index_map : (int,blk_id,blk_id,t)pre_map_ops)
      ~(with_inode    : ((fid,blk_id)inode,t)with_state)
      ~(alloc         : unit -> (blk_id,t)m)
  =
</pre>
%}

TODO:
- b-tree to implement delete_after (or some alternative approach)
- validate against "normal" file semantics
- ensure the tests are run by some top-level test executable
- implement freeing of blocks after truncate
- maybe worth distinguishing the blk_id used by the map from the blk_id used to store data
*)

open Int_like
open Buffers

type 'fid file_id = { fid:'fid }

module Inode = struct
  type 'blk_id btree_root = { btree_root: 'blk_id }


  type ('fid,'blk_id) inode = { 
    file_size : size; (* in bytes *)
    blk_index_map_root : 'blk_id (*  btree_root *)
  }
end
open Inode


(** [Pre_map_ops]: because the file root is stored in the inode, we need to access
   the B-tree using explicit root passing (and then separately update
   the inode); this is to ensure that the inode update is atomic (via
   with_inode). We could store the root in a separate block but this
   is a bit too inefficient. *)
module Pre_map_ops = struct
  (* we don't expose leaf and frame here *)
  type ('k,'v,'r,'t) pre_map_ops = {
    find         : r:'r -> k:'k -> ('v option,'t) m;
    insert       : r:'r -> k:'k -> v:'v -> ('r option,'t) m;
    delete       : r:'r -> k:'k -> ('r,'t) m;
    delete_after : r:'r -> k:'k -> ('r,'t) m; (* delete all entries for keys > r; used for truncate *)
  }
end
open Pre_map_ops


(*
type ('buf,'t) file_ops = {
  size   : unit -> (size,'t)m;
  pwrite : src:'buf -> src_off:offset -> src_len:len_ -> dst_off:offset -> (size,'t)m;
  pread  : src_off:offset -> src_len:len_ -> dst:'buf -> dst_off:offset -> (size,'t)m
}
*)


type pread_error = Pread_error of string

type pwrite_error = Pwrite_error of string

(** Standard file operations, pwrite, pread, size and truncate.

NOTE we expect buf to be string for the functional version; for
   mutable buffers we may want to pass the buffer in as a parameter? 

FIXME for pwrite, we always return src_len since all bytes are written (unless there is an error of course). So perhaps return unit.

For pread, we always return a buffer of length len.
*)
type ('buf,'t) file_ops = {
  size     : unit -> (size,'t)m;
  pwrite   : src:'buf -> src_off:offset -> src_len:len -> 
    dst_off:offset -> ((size,pwrite_error)result,'t)m;
  pread    : off:offset -> len:len -> (('buf,pread_error)result,'t)m;
  truncate : size:size -> (unit,'t)m
}


type ('a,'b) iso = {
  a_to_b: 'a -> 'b;
  b_to_a: 'b -> 'a
}

(** Check the arguments to pread *)
let pread_check = 
  let module A = struct

    let pread_check_1 ~size:Int_like.{size} ~off:{off} ~len:{len} = 
      assert(off >=0);
      assert(len >=0);  
      match () with
      | _ when off < 0                 -> `Off_neg
      | _ when len < 0                 -> `Len_neg
      | _ when off > size && len=0     -> `Off_gt_sz_and_len_0 (* not an error *)
      | _ when off > size              -> `Off_gt_sz_and_pos_len
      | _ when off+len > size && len=0 -> `Offlen_gt_sz_and_len_0
      | _ when off+len > size          -> `Offlen_gt_sz_and_pos_len
      | _ -> `Ok

    let pread_check_2 ~size ~off ~len = 
      pread_check_1 ~size ~off ~len |> function
      | `Off_neg | `Len_neg -> `Error `Off_or_len
      | `Off_gt_sz_and_len_0 | `Offlen_gt_sz_and_len_0 -> `Ok `Gt_sz_len_0
      | `Off_gt_sz_and_pos_len | `Offlen_gt_sz_and_pos_len -> `Error `Gt_sz_pos_len
      | `Ok -> `Ok `Unit

    let pread_check ~size ~off ~len = 
      pread_check_2 ~size ~off ~len |> function
      | `Ok _ -> Ok ()
      | `Error `Off_or_len -> Error "off<0 or len<0"
      | `Error `Gt_sz_pos_len -> Error "off+len>size and len>0"
  end
  in A.pread_check


(** Check arguments to pwrite *)
let pwrite_check ~buf_ops =
  let module A = struct
    let pwrite_check_1 : src:'buf -> src_off:offset -> src_len:len -> dst_off:offset -> _ = 
      fun ~src ~src_off:{off=src_off} ~src_len:{len=src_len} ~dst_off:{off=dst_off} ->
        match () with
        | _ when src_off < 0 -> `Err `Src_off_neg
        | _ when dst_off < 0 -> `Err `Dst_off_neg
        | _ when src_off+src_len > (buf_ops.buf_size src).size -> `Err `Einval
        | _ -> `Ok

    let pwrite_check ~src ~src_off ~src_len ~dst_off = 
      pwrite_check_1 ~src ~src_off ~src_len ~dst_off |> function
      | `Err `Src_off_neg -> Error "src_off<0"
      | `Err `Dst_off_neg -> Error "dst_off<0"
      | `Err `Einval -> Error "src_off+src_len>src.size"
      | `Ok -> Ok ()
  end
  in A.pwrite_check

(** Abstract model of blitting from blks to buffer.

Spec: a seq of blits equiv to a single blit, but at most 1 blit is not
block aligned, and at most 2 blits are not blk sized; also, minimal
number of blits (no overlapping blits). 

This version assumes that the
file is backed by a map from blk_index -> blk_id (although for the
test code, we ignore blk_id and just work directly with bytes.)

- read_blk: given a block index, we look up the actual blk_id in
  the map, and read that block; return empty blk if no blk; ignore size

- alloc_and_write_blk: for the situation where we
  definitely want to allocate a new blk; we have a blk_index, a blk,
  and we allocate a blk_id, write the blk, insert the (index,blk_id)
  into the map, then return the blk_id

- rewrite_blk_or_alloc: given a blk_index and a blk_id (of the previous
  version of the blk) and the new blk, we attempt to rewrite that blk
  directly; if for some reason we cannot mutate the old blk, we
  allocate a new blk_id, write the new blk, insert the
  (index,blk_id) into the map

- truncate n: drop the blk-index map entries for all blocks after byte index n

For the usage with file_impl, we assume the inode is locked (so we can update the inode using set_state).
 *)
module Iter_block_blit = struct

  (* These versions assume that the arguments are all "ok" *)
  type ('buf,'t) ops = {
    pwrite   : src:'buf -> src_off:offset -> src_len:len -> dst_off:offset -> (size,'t)m;
    pread    : off:offset -> len:len -> ('buf,'t)m;
  }

  (** NOTE we want [make] to be (as far as possible) independent
      of the updates to the blk-index map. NOTE that blk_id is
      essentially completely abstract here, and only used when rewriting. *)
  let make (type buf blk_id blk i t) 
        ~monad_ops 
        ~(buf_ops              : buf buf_ops)
        ~(blk_ops              : blk blk_ops)
        ~(int_index_iso        : (int,i)iso)
        ~(read_blk             : i index_ -> (blk_id*blk,t)m)  (* blk_id is used only when rewriting *)
        ~(alloc_and_write_blk  : i index_ -> blk -> (unit,t)m)
        ~(rewrite_blk_or_alloc : i index_ -> blk_id*blk -> (unit,t)m) (* FIXME return bool? unit? *)
    = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in

    let {a_to_b=int_to_i; b_to_a=i_to_int} = int_index_iso in

    let inc i = i |> i_to_int |> (fun x -> x+1) |> int_to_i in

    (* This version assumes that the arguments pass the pread_check *)
    let pread ~off:{off=off0} ~len:{len=len0} =
      (* FIXME we may want to launch threads for each block 
          read, then wait on all these threads to finish *)
      (* FIXME this block-aligned blitting should probably be factored out and tested *)
      let rec loop (buf,buf_off,blk_n,blk_off,len_remain) = (
        assert(blk_off < blk_sz);
        assert(blk_off >= 0);
        let blit_blk_to_buf ~(blk:blk) ~blk_off ~len =
          assert(blk_off.off + len.len <= blk_sz);
          blk |> blk_ops.to_string |> fun src -> 
          buf_ops.blit_string_to_buf ~src ~src_off:blk_off
            ~src_len:len ~dst:buf ~dst_off:{off=buf_off} |> fun buf ->
          buf
        in
        match len_remain with 
        | 0 -> return buf
        | _ -> (
            (* try to read the blk, add to buf, cont if len is > 0 *)
            read_blk {index=blk_n} >>= fun (_,blk) ->
            let can_write (* in_this_buf *) = blk_sz - blk_off in
            (* Printf.printf "can_write: %d\n" can_write; *)
            assert(can_write <= blk_sz);
            let len = min len_remain can_write in
            blit_blk_to_buf ~blk ~blk_off:{off=blk_off} ~len:{len} |> fun buf ->
            let len_remain' = len_remain - len in
            match () with 
            | _ when len_remain' = 0 -> return buf
            | _ when blk_off+len=blk_sz -> 
              loop (buf,buf_off+len,inc blk_n,0,len_remain')
            | _ -> assert false))
      in
      let buf = buf_ops.buf_create len0 in
      loop (buf,0,int_to_i (off0 / blk_sz), off0 mod blk_sz,len0)
    in

    (*   pwrite : src:'buf -> src_off:offset -> src_len:len -> dst_off:offset -> (size,'t)m; *)
    let pwrite ~(src:buf) ~src_off:{off=src_off} ~src_len:{len=src_len0} ~dst_off:{off=dst_off} = 
      let module S = StringLabels in
      (* NOTE this is slightly different, because for full blocks we don't need to read then write *)
      let src : string = buf_ops.to_string src in
      let rec loop (src_off,len_remain,blk_n,blk_off) = 
        (* Printf.printf "pwrite: src_off=%d len_remain=%d  blk_n=%d  blk_off=%d\n" src_off len_remain (i_to_int blk_n) blk_off; *)
        match len_remain with 
        | 0 -> return {size=src_len0}
        | _ -> 
          match blk_off = 0 && len_remain >= blk_sz with
          | true -> (
              (* block aligned; write and continue; FIXME we may need to adjust size? *)
              let len = blk_sz in
              let blk = blk_ops.of_string (S.sub src ~pos:src_off ~len) in
              alloc_and_write_blk {index=blk_n} blk >>= fun _r -> 
              (* don't need to record the blkid? *)
              loop (src_off+len,len_remain-len,inc blk_n,0))
          | false -> (
              (* have to read blk then update *)
              read_blk {index=blk_n} >>= fun (rr,blk) ->
              blk |> blk_ops.to_string |> buf_ops.of_string |> fun blk ->
              let len = min (blk_sz - blk_off) len_remain in
              buf_ops.blit_string_to_buf ~src ~src_off:{off=src_off} ~src_len:{len} 
                ~dst:blk ~dst_off:{off=blk_off} |> fun blk ->
              rewrite_blk_or_alloc {index=blk_n} (rr,blk |> buf_ops.to_string |> blk_ops.of_string) 
              >>= fun () (* _ropt *) ->
              let len_remain' = len_remain - len in
              match len_remain' = 0 with
              | true -> return {size=src_len0}
              | false -> 
                assert(len=blk_sz-blk_off);
                loop (src_off+len,len_remain',inc blk_n,0))
      in
      let blk_n = dst_off / blk_sz in
      let blk_off = dst_off mod blk_sz in
      loop (src_off,src_len0,int_to_i blk_n,blk_off)
    in
    { pread; pwrite }

  let _ = make

  let test () =
    Printf.printf "Iter_block_blit: tests start...\n";
    let monad_ops = imperative_monad_ops in
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let to_m,of_m = Imperative.(to_m,of_m) in
    let buf_ops = bytes_buf_ops in
    let blk_ops = Common_blk_ops.String_.make ~blk_sz:(Blk_sz.of_int 2) in 
    (* FIXME this needs to have blk_sz 2; FIXME perhaps make it
       clearer that Common_blk_ops.string_blk_ops has size 4096 *)
    let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
    let with_bytes buf = 
      let buf_size = (buf_ops.buf_size buf).size in
      assert(buf_size mod blk_sz = 0);
      let buf = ref buf in
      let read_blk {index=i} = to_m (
          buf_ops.buf_to_string ~src:!buf ~off:{off=(i*blk_sz)} ~len:{len=blk_sz} |> fun s ->
          (-1,s)) in
      let rewrite_blk_or_alloc {index=i} (_blk_id,blk) = to_m (
          buf_ops.blit_string_to_buf ~src:blk ~src_off:{off=0} ~src_len:{len=blk_sz}
            ~dst:!buf ~dst_off:{off=i*blk_sz} |> fun buf' ->
          buf := buf'; ())
      in
      let alloc_and_write_blk i blk = 
        rewrite_blk_or_alloc i (-1,blk) >>= fun _ -> 
        return ()
      in
      let int_index_iso = {a_to_b=(fun x -> x); b_to_a=(fun x -> x) } in
      let { pread; pwrite; _ } = 
        make ~monad_ops ~buf_ops ~blk_ops ~int_index_iso ~read_blk 
          ~alloc_and_write_blk ~rewrite_blk_or_alloc
      in
      pread,pwrite,buf
    in
    of_m (
      with_bytes (buf_ops.of_string "abcdef") |> fun (pread,pwrite,buf) -> 
      pread ~off:{off=0} ~len:{len=0} >>= fun b -> 
      assert(buf_ops.to_string b = "");
      pread ~off:{off=0} ~len:{len=1} >>= fun b -> 
      assert(buf_ops.to_string b = "a");
      pread ~off:{off=0} ~len:{len=2} >>= fun b -> 
      assert(buf_ops.to_string b = "ab");
      pread ~off:{off=0} ~len:{len=3} >>= fun b -> 
      assert(buf_ops.to_string b = "abc");
      pread ~off:{off=0} ~len:{len=6} >>= fun b -> 
      assert(buf_ops.to_string b = "abcdef");
      let src = (buf_ops.of_string "xyz") in
      pwrite ~src ~src_off:{off=0} ~src_len:{len=1} ~dst_off:{off=0} >>= fun _sz -> 
      assert(buf_ops.to_string (!buf) = "xbcdef");
      pwrite ~src ~src_off:{off=0} ~src_len:{len=2} ~dst_off:{off=0} >>= fun _sz -> 
      assert(buf_ops.to_string (!buf) = "xycdef");
      pwrite ~src ~src_off:{off=0} ~src_len:{len=3} ~dst_off:{off=0} >>= fun _sz -> 
      assert(buf_ops.to_string (!buf) = "xyzdef");
      buf:=buf_ops.of_string "abcdef";
      pwrite ~src ~src_off:{off=0} ~src_len:{len=1} ~dst_off:{off=0} >>= fun _sz -> 
      assert(buf_ops.to_string (!buf) = "xbcdef");
      buf:=buf_ops.of_string "abcdef";
      pwrite ~src ~src_off:{off=1} ~src_len:{len=1} ~dst_off:{off=1} >>= fun _sz -> 
      assert(buf_ops.to_string (!buf) = "aycdef" |> fun b -> 
             b || (Printf.printf "buf: %s\n" (buf_ops.to_string (!buf)); false));      
      buf:=buf_ops.of_string "abcdef";
      pwrite ~src ~src_off:{off=0} ~src_len:{len=3} ~dst_off:{off=1} >>= fun _sz -> 
      assert(buf_ops.to_string (!buf) = "axyzef");
      return ());
    Printf.printf "Iter_block_blit: tests end!\n"
  [@@ocaml.warning "-8"]

end

(* FIXME at the moment, this assumes that we can rewrite file blocks as we wish *)

(* FIXME in order to implement with_inode, we need to somehow keep
   track of which inodes are currently in memory; so we need an inode
   cache with atomic "with"; can't just resurrect from disk all the
   time since we may get more than one inode for the same file. This
   in turn means that inodes which are locked must not be evicted
   until they are unlocked. If we want to evict an inode, we need to
   mark that it cannot be locked again.  Perhaps the inode is stored
   with other file info (descriptor etc).  Could maybe detect an error
   when an inode is locked for more than a certain length of time. *)

(** NOTE The pread,pwrite functions that result will lock the inode
    whilst executing, in order to udpate the B-tree root 
{%html: 
<pre>
let make (type fid blk blk_id t) 
      ~monad_ops
      ~(blk_ops       : blk blk_ops)
      ~(blk_dev_ops   : (blk_id,blk,t) blk_dev_ops)
      ~(blk_index_map : (int,blk_id,blk_id,t)pre_map_ops)
      ~(with_inode    : ((fid,blk_id)inode,t)with_state)
      ~(alloc         : unit -> (blk_id,t)m)
  =
</pre>
%}
*)
let make (type fid blk blk_id t) 
      ~monad_ops
      ~(blk_ops       : blk blk_ops)
      ~(blk_dev_ops   : (blk_id,blk,t) blk_dev_ops)
      ~(blk_index_map : (int,blk_id,blk_id,t)pre_map_ops)
      ~(with_inode    : ((fid,blk_id)inode,t)with_state)
      ~(alloc         : unit -> (blk_id,t)m)
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { with_state } = with_inode in
  let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
  let empty_blk () = blk_ops.of_string (String.make blk_sz (Char.chr 0)) in 
  (* assumes functional blk impl ?; FIXME blk_ops pads string automatically? *)
  let bind = blk_index_map in
  let dev = blk_dev_ops in
  let size () = 
    with_state (fun ~state ~set_state:_ -> 
      return state.file_size)
  in
  let truncate_blk ~blk ~blk_off = 
    blk |> blk_ops.to_string |> fun blk -> String.sub blk 0 blk_off |> blk_ops.of_string
  in
  let truncate ~size = 
    (* FIXME the following code is rather hacky, to say the least *)
    with_state (fun ~state:inode ~set_state -> 
      match size.Int_like.size >= inode.file_size.size with
      | true -> 
        (* no need to drop blocks *)
        set_state{inode with file_size=size}
      | false -> 
        (* FIXME check the maths of this - if size is 0 we may want to have no entries in blk_index *)
        (* drop all blocks after size/blk_sz FIXME would be nice to have
           "delete from"; perhaps we list the contents of the index_map
           and remove the relevant keys? or a monadic fold or iteration? *)
        (* also need to zero out the bytes in the final block beyond size.size *)
        let i,blk_off = size.size / blk_sz, size.size mod blk_sz in
        let r = inode.blk_index_map_root in
        bind.delete_after ~r ~k:i >>= fun r ->
        (
          match blk_off > 0 with
          | true -> (
              (* read blk and zero out suffix, then rewrite *)
              bind.find ~r ~k:i >>= function
              | None -> return ()
              | Some blk_id -> 
                dev.read ~blk_id >>= fun blk ->
                truncate_blk ~blk ~blk_off |> fun blk ->
                (* FIXME we need a rewrite here *)
                dev.write ~blk_id ~blk)
          | false -> return ()
        ) >>= fun () -> 
        set_state {file_size=size; blk_index_map_root=r})
  in
  let read_blk {index=(i:int)} =
    with_state (fun ~state:inode ~set_state -> 
      bind.find ~r:inode.blk_index_map_root ~k:i >>= function
      | None -> (
          alloc () >>= fun r -> 
          let blk = empty_blk () in
          (* this ensures that every blk_id corresponds to a blk that has been written *)
          dev.write ~blk_id:r ~blk >>= fun () ->
          (* NOTE need to add to index map *)
          bind.insert ~r:inode.blk_index_map_root ~k:i ~v:r >>= fun ropt ->
          match ropt with
          | None -> return (r,blk)
          | Some blk_index_map_root -> 
            set_state {inode with blk_index_map_root} >>= fun () ->
            return (r,blk))
      | Some blk_id -> 
        dev.read ~blk_id >>= fun blk ->
        return (blk_id,blk))
  in
  let _ = read_blk in
  let alloc_and_write_blk {index=i} blk = 
    with_state (fun ~state:inode ~set_state ->       
      alloc () >>= fun blk_id -> 
      dev.write ~blk_id ~blk >>= fun () ->
      bind.insert ~r:inode.blk_index_map_root ~k:i ~v:blk_id >>= fun opt ->
      match opt with
      | None -> return ()
      | Some blk_index_map_root -> 
        set_state {inode with blk_index_map_root} >>= fun () -> 
        return ())
  in
  let buf_ops = bytes_buf_ops in
  let rewrite_blk_or_alloc {index=_i} (blk_id,blk) = 
    (* FIXME at the moment, this always succeeds FIXME note we don't
       attempt to reinsert into B-tree (concurrent modification by
       another thread may result in unusual behaviour) FIXME maybe
       need another layer about blk_dev, which allows rewrite *)
    (* FIXME if we have not already inserted index -> blk_id into the map, we probably should *)
    (* let blk_id = i |> Blk_id_as_int.of_int in *)
    dev.write ~blk_id ~blk >>= fun () -> return ()
  in
  let int_index_iso = {a_to_b=(fun x -> x); b_to_a=(fun x -> x) } in
  let Iter_block_blit.{ pread;pwrite } = 
    Iter_block_blit.make ~monad_ops ~buf_ops ~blk_ops ~int_index_iso ~read_blk 
      ~alloc_and_write_blk ~rewrite_blk_or_alloc
  in
  let pread ~off ~len =     
    with_state (fun ~state:inode ~set_state:_ ->
      let size = inode.file_size in
      (* don't attempt to read beyond end of file *)
      let len = {len=min len.len (size.size - off.off)} in
      pread_check ~size ~off ~len |> function 
      | Ok () -> pread ~off ~len >>= fun x -> return (Ok x)
      | Error s -> return (Error (Pread_error s)))
  in
  let pwrite ~src ~src_off ~src_len ~dst_off =
    with_state (fun ~state:inode ~set_state ->
      let size = inode.file_size in
      pwrite_check ~buf_ops ~src ~src_off ~src_len ~dst_off |> function
      | Error s -> return (Error (Pwrite_error s))
      | Ok () -> pwrite ~src ~src_off ~src_len ~dst_off >>= fun x -> 
        (* now may need to adjust the size of the file *)
        begin
          let size' = dst_off.off+src_len.len in
          match size' > size.size with
          | true -> set_state {inode with file_size={size=size'}}
          | false -> return ()
        end >>= fun () ->
        return (Ok x)
    )
  in
  { size; truncate; pread; pwrite }

let _ = make

(* NOTE should test this on top of an in-mem store blk_index *)

let test () = 
  let module A = struct

    let monad_ops = Tjr_monad.imperative_monad_ops

    let return = monad_ops.return

    let blk_sz = Blk_sz.of_int 2

    let blk_ops = Common_blk_ops.String_.make ~blk_sz

    let blk_layer = 
      Common_blk_layers.blk_layer_string_mem ~blk_sz 

    (* type blk = string *)

    let make_blk_dev_ops = blk_layer.blk_dev_ops ~monad_ops
                             
    let _ = make_blk_dev_ops

    (* the blk dev is modelled by a map from blk_id to blk *)
    
    type blk_id = Blk_id_as_int.blk_id

    module M = Tjr_map.With_pervasives_compare
        
    (* type blk_dev_map = (blk_id,blk)M.map_with_pervasives_compare *)
    
    (* [@@@ocaml.warning "-34"] *)

    let blk_dev_ref = ref (M.empty ())

    let with_blk_dev = with_imperative_ref ~monad_ops blk_dev_ref

    let blk_dev_ops = make_blk_dev_ops ~with_state:with_blk_dev
                        
    let _ = blk_dev_ops


    (* blk index map *)

    type blk_index = (int,blk_id)M.map_with_pervasives_compare

    let blk_index_ref = ref ((M.empty ()):blk_index)


    (* the file inode *)

    let inode_ref = ref {file_size={size=0};blk_index_map_root=(Blk_id_as_int.of_int (-1))}

    let with_inode = with_imperative_ref ~monad_ops inode_ref

    let _ = with_inode


    let min_free_blk_id = ref 0

    let alloc () = 
      !min_free_blk_id |> fun i -> 
      incr min_free_blk_id;
      return (Blk_id_as_int.of_int i)
      
    let _ = make_blk_dev_ops

    let blk_index_map = {
      find=(fun ~r:_ ~k -> M.find_opt k !blk_index_ref |> return);
      insert=(fun ~r:_ ~k ~v -> 
          M.add k v !blk_index_ref |> fun x -> 
          blk_index_ref:=x; 
          return None);
      delete=(fun ~r ~k -> 
        M.remove k !blk_index_ref |> fun x ->
        blk_index_ref:=x;
        return r);
      delete_after=(fun ~r ~k -> 
        M.split k !blk_index_ref |> fun (_,_,after) -> 
        M.iter (fun k _v -> M.remove k !blk_index_ref |> fun x -> blk_index_ref:=x) after;
        return r)
    }
      
    
    let file_ops = make ~monad_ops ~blk_ops ~blk_dev_ops ~blk_index_map ~with_inode ~alloc

    let _ = file_ops

    let { truncate=_; pread; pwrite; size=_ } = file_ops

    open Imperative

    let _ = 
      Printf.printf "File_impl: tests starting...\n";
      (* read from empty file *)
      pread ~off:{off=0} ~len:{len=100} |> of_m |> fun (Ok buf) ->
      assert(bytes_buf_ops.to_string buf = "");  (* file initially empty *)

      (* write "tom" *)
      let src = bytes_buf_ops.of_string "tom" in
      pwrite ~src ~src_off:{off=0} ~src_len:{len=3} ~dst_off:{off=0} |> of_m |> fun (Ok Int_like.{size=_}) ->
      (* Printf.printf "%d\n" size; *)
      pread ~off:{off=0} ~len:{len=3} |> of_m |> fun (Ok buf) ->
      assert(
        bytes_buf_ops.to_string buf = "tom" || (
          Printf.printf "|%s|\n" (bytes_buf_ops.to_string buf); false));

      (* try to read 10 bytes from file *)
      pread ~off:{off=0} ~len:{len=10} |> of_m |> fun (Ok buf) ->
      assert(bytes_buf_ops.to_string buf = "tom");

      (* make file contain 20xNULL *)
      let src = bytes_buf_ops.of_string (String.make 20 '\x00') in
      pwrite ~src ~src_off:{off=0} ~src_len:{len=20} ~dst_off:{off=0} |> of_m |> fun (Ok Int_like.{size=_}) ->

      (* write "homas" at off 1 *)
      let src = bytes_buf_ops.of_string "thomas" in
      pwrite ~src ~src_off:{off=1} ~src_len:{len=5} ~dst_off:{off=1} |> of_m |> fun (Ok Int_like.{size=_}) ->

      (* read 10 bytes *)
      pread ~off:{off=0} ~len:{len=10} |> of_m |> fun (Ok buf) ->
      assert(bytes_buf_ops.to_string buf = "\x00homas\x00\x00\x00\x00");
      Printf.printf "File_impl: tests end!\n";
      ()
      [@@warning "-8"]
      
  end
  in
  ()
