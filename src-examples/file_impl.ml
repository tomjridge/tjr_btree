(** A simple file implementation. Primarily this is here so that we
   can use it to marshall the free list *)

type 'fid file_id = { fid:'fid }

module Inode = struct
  type 'blk_id btree_root = { btree_root: 'blk_id }

  type ('fid,'blk_id) inode = { 
    file_size : int; (* in bytes *)
    file_root : 'blk_id btree_root
  }
end
open Inode



(** Types for documentation purposes *)
module T = struct
  type 'int size_ = {
    size:'int
  } [@@unboxed]

  type size = int size_

  type 'int offset_ = {
    off:'int
  } [@@unboxed]

  type offset = int offset_

  type len = {
    len:int
  } [@@unboxed]
end
open T


(*
type ('buf,'t) file_ops = {
  size   : unit -> (size,'t)m;
  pwrite : src:'buf -> src_off:offset -> src_len:len_ -> dst_off:offset -> (size,'t)m;
  pread  : src_off:offset -> src_len:len_ -> dst:'buf -> dst_off:offset -> (size,'t)m
}
*)

(** NOTE we expect buf to be string for the functional version; for
   mutable buffers we may want to pass the buffer in as a parameter? *)
type ('buf,'t) file_ops = {
  size   : unit -> (size,'t)m;
  pwrite : src:'buf -> src_off:offset -> src_len:len -> dst_off:offset -> (size,'t)m;
  pread  : off:offset -> len:len -> ('buf,'t)m
}

(** "mutable", fixed size buffers; it is an error to attempt to write
beyond the end; prefer a functional repr. for now FIXME change for
production *)
type 'buf buf_ops = {
  buf_create         : int -> 'buf;
  buf_size           : 'buf -> size;

  buf_to_string      : src:'buf -> off:offset -> len:len -> string; 
  to_string          : 'buf -> string;
  (* to string? also, a version without off and len*)

  (* string_to_buf      : string -> 'buf; (\* FIXME of string? *\) *)
  of_string          : string -> 'buf;
  blit_string_to_buf : src:string -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
  blit_bytes_to_buf  :  src:bytes -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
}

(** Abstract buffer type, with underlying string representation; FIXME
   move to mutable implementation for production *)
module Abstract_buf : sig
  type abstract_buf
  val buf_ops: abstract_buf buf_ops 
  (* val string_buf_ops : string buf_ops *)
end = struct
  type abstract_buf = string

  let string_buf_ops = 
    let module B = BytesLabels in
    let buf_create n = String.make n (Char.chr 0) in
    let buf_to_string ~src ~off:{off} ~len:{len} =
      String.sub src off len
    in
    let of_string s = s in
    let to_string s = s in
    let blit_bytes_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
      let {off=src_pos} = src_off in
      let {len} = src_len in
      let dst : bytes = dst |> B.of_string in
      let {off=dst_pos} = dst_off in
      assert (src_pos + len < B.length src);
      assert (dst_pos + len < B.length dst);
      B.blit ~src ~src_pos ~dst ~dst_pos ~len;
      B.to_string dst
    in
    let blit_string_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
      blit_bytes_to_buf ~src:(B.of_string src) ~src_off ~src_len ~dst ~dst_off
    in
    let buf_size buf = String.length buf |> fun size -> {size} in
    {buf_create;buf_size;buf_to_string;of_string;to_string;blit_bytes_to_buf; blit_string_to_buf}

  let buf_ops = string_buf_ops
end


(** Abstract buffer type, with underlying bytes representation; FIXME use this in production *)
module Bytes_buf : sig
  type bytes_buf
  val buf_ops: bytes_buf buf_ops 
end = struct
  type bytes_buf = bytes

  let bytes_buf_ops = 
    let module B = BytesLabels in
    let buf_create n = B.make n (Char.chr 0) in
    let buf_to_string ~src ~off:{off} ~len:{len} =
      B.sub_string src ~pos:off ~len
    in
    let of_string s = B.of_string s in
    let to_string s = B.to_string s in
    let blit_bytes_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
      let {off=src_pos} = src_off in
      let {len} = src_len in
      (* let dst = dst |> B.of_string in *)
      let {off=dst_pos} = dst_off in
      assert (src_pos + len < B.length src);
      assert (dst_pos + len < B.length dst);
      B.blit ~src ~src_pos ~dst ~dst_pos ~len;
      dst
    in
    let blit_string_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
      blit_bytes_to_buf ~src:(Bytes.unsafe_of_string src) ~src_off ~src_len ~dst ~dst_off
    in
    let buf_size buf = B.length buf |> fun size -> {size} in
    {buf_create;buf_size;buf_to_string;of_string;to_string;blit_bytes_to_buf; blit_string_to_buf}

  let buf_ops = bytes_buf_ops
end

let bytes_buf_ops = Bytes_buf.buf_ops

(* FIXME also want a bigstring version *)


open Tjr_fs_shared.Shared_map_ops.Fid_map_ops

(** Abstract model of blitting from blks to buffer.

Spec: a seq of blits equiv to a single blit, but at most 1 blit is not
block aligned, and at most 2 blits are not blk sized; also, minimal
number of blits (no overlapping blits)
 *)
module Iter_block_blit = struct
  let make (type buf blk_id blk t) 
        ~monad_ops 
        ~(buf_ops    : buf buf_ops)
        ~(blk_ops    : blk blk_ops)
        ~(read_file_blk : int -> (blk_id*blk,t)m)
        ~(rewrite_file_blk: int -> blk_id*blk -> (blk_id option,t)m)
        ~(alloc_and_write_file_blk: int -> blk -> (blk_id,t)m)
    = 
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in

    let pread ~off ~len =
      let off0,len0 = off.off,len.len in
      let off,len = (),() in
      (* FIXME we may want to launch threads for each block 
          read, then wait on all these threads to finish *)
      (* FIXME this block-aligned blitting should probably be factored out and tested *)
      let rec loop (buf,buf_off,blk_n,blk_off,len_remain) = (
        assert(blk_off < blk_sz);
        let blit_blk_to_buf ~(blk:blk) ~blk_off ~len =
          blk |> blk_ops.to_string |> fun src -> 
          buf_ops.blit_string_to_buf ~src ~src_off:blk_off
            ~src_len:len ~dst:buf ~dst_off:{off=buf_off} |> fun buf ->
          buf
        in
        match len_remain with 
        | 0 -> return buf
        | _ -> (
            (* we are blk aligned, so try to read the blk and
                   add to buf and cont if len is > 0 *)
            read_file_blk blk_n >>= fun (_,blk) ->
            let can_write (* in_this_buf *) = blk_sz - blk_off in
            let len = min len_remain can_write in
            blit_blk_to_buf ~blk ~blk_off:{off=blk_off} ~len:{len} |> fun buf ->
            let len_remain' = len_remain - len in
            match () with 
            | _ when len_remain' = 0 -> return buf
            | _ when blk_off+len=blk_sz -> 
              loop (buf,buf_off+len,blk_n+1,0,len_remain')
            | _ -> assert false))
      in
      let buf = buf_ops.buf_create len0 in
      loop (buf,0,off0 / blk_sz, off0 mod blk_sz,len0)
    in

    (*   pwrite : src:'buf -> src_off:offset -> src_len:len -> dst_off:offset -> (size,'t)m; *)
    let pwrite ~(src:buf) ~src_off ~src_len ~dst_off = 
      let module S = StringLabels in
      let src_len0 = src_len.len in
      (* NOTE this is slightly different, because for full blocks we don't need to read then write *)
      let src : string = 
        buf_ops.buf_to_string ~src ~off:{off=0} ~len:{len=src_len0} (* FIXME inefficient *)
      in
      let rec loop (src_off,len_remain,blk_n,blk_off) = 
        match len_remain with 
        | 0 -> return {size=src_len0}
        | _ -> 
          match blk_off = 0 && len_remain >= blk_sz with
          | true -> (
              (* block aligned; write and continue; FIXME we may need to adjust size? *)
              let blk = blk_ops.of_string (S.sub src ~pos:src_off ~len:blk_sz) in
              alloc_and_write_file_blk blk_n blk >>= fun _r -> (* don't need to record the blkid *)
              loop (src_off+blk_sz,len_remain-blk_sz,blk_n+1,0))
          | false -> (
              (* have to read blk then update *)
              read_file_blk blk_n >>= fun (r,blk) ->
              blk |> blk_ops.to_string |> buf_ops.of_string |> fun blk ->
              let len = min (blk_sz - blk_off) len_remain in
              buf_ops.blit_string_to_buf ~src ~src_off:{off=src_off} ~src_len:{len} 
                ~dst:blk ~dst_off:{off=blk_off} |> fun blk ->
              rewrite_file_blk blk_n (r,blk |> buf_ops.to_string |> blk_ops.of_string) >>= fun _ropt ->
              let len_remain' = len_remain - len in
              match len_remain' = 0 with
              | true -> return {size=src_len0}
              | false -> 
                assert(len=blk_sz-blk_off);
                loop (src_off+len,len_remain',blk_n+1,0))
      in
      let dst_off = dst_off.off in
      let blk_n = dst_off / blk_sz in
      let blk_off = dst_off mod blk_sz in
      loop (src_off.off,src_len0,blk_n,blk_off)
    in
      
    let size () = failwith __LOC__ in
    { size; pread; pwrite }

end

(* FIXME at the moment, this assumes that we can rewrite file blocks as we wish *)
let make (type fid blk blk_id t) 
      ~monad_ops
      ~(blk_ops    : blk blk_ops)
      ~(blk_dev_ops: (blk_id,blk,t) blk_dev_ops)
      (* ~(buf_ops    : buf buf_ops) *)
      ~(btree_ops  : (int,blk_id,t)fid_map_ops)
      ~(with_inode : ((fid,blk_id)inode,t)with_state)
      ~(alloc      : unit -> (blk_id,t)m)
      _
  =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { with_state } = with_inode in
  let blk_sz = blk_ops.blk_sz |> Blk_sz.to_int in
  let empty_blk = blk_ops.of_string (String.make blk_sz (Char.chr 0)) in 
  (* assumes functional blk impl; FIXME blk_ops pads string automatically? *)
  let size () = 
    with_state (fun ~state ~set_state:_ -> 
      return {size=state.file_size})
  in
  let bt = btree_ops in
  let dev = blk_dev_ops in
  let read_file_blk i =
    bt.find i >>= function
    | None -> 
      alloc () >>= fun r -> 
      return (r,empty_blk)
    | Some blk_id -> 
      dev.read ~blk_id >>= fun blk ->
      return (blk_id,blk)          
  in
  let alloc_and_write_file_blk i blk = 
    alloc () >>= fun r -> 
    dev.write ~blk_id:r ~blk >>= fun () ->
    bt.insert i r >>= fun () ->
    return r
  in
  let buf_ops = bytes_buf_ops in
  let rewrite_file_blk _i (blk_id,blk) = 
    (* FIXME at the moment, this always succeeds FIXME note we don't
       attempt to reinsert into B-tree (concurrent modification by
       another thread may result in unusual behaviour) *)
    dev.write ~blk_id ~blk >>= fun () -> return None
  in
  let { pread;pwrite;_ } = 
    Iter_block_blit.make ~monad_ops ~buf_ops ~blk_ops ~read_file_blk 
      ~rewrite_file_blk ~alloc_and_write_file_blk
  in
  { size; pread; pwrite }
  
