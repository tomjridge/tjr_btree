(* FIXME move this elsewhere eg fs_shared (and combine with other similar modules) *)
open Int_like 

(** "mutable", fixed size buffers; it is an error to attempt to write
beyond the end; prefer a functional repr. for now FIXME change for
production *)
type 'buf buf_ops = {
  buf_create         : int -> 'buf;
  buf_size           : 'buf -> size;

  buf_to_string      : src:'buf -> off:offset -> len:len -> string; 
  to_string          : 'buf -> string;
  of_string          : string -> 'buf;

  blit_string_to_buf : src:string -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
  blit_bytes_to_buf  : src:bytes -> src_off:offset -> src_len:len -> dst:'buf -> dst_off:offset -> 'buf;
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
module Unsafe_bytes_buf : sig
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


module Safe_bytes_buf : sig
  type bytes_buf
  val buf_ops: bytes_buf buf_ops 
end = struct
  
  (* The idea is to use "buffer passing", but to mark old buffers as
     "not ok". Attempts to access "not ok" buffers results in a
     runtime error. In some sense, the reference to the bytes (via the
     record... but bytes is mutable anyway) is the owner of the bytes,
     until some modifcation takes place. *)
  type bytes_buf = {
    mutable ok:bool;
    bytes: bytes
  }

  let wf buf = assert(buf.ok)

  (* type t = bytes_buf *)

  let bytes_buf_ops = 
    let module B = BytesLabels in
    let buf_create n = B.make n (Char.chr 0) |> fun bytes -> {ok=true;bytes} in
    let buf_to_string ~src ~off:{off} ~len:{len} =
      B.sub_string src.bytes ~pos:off ~len
    in
    let of_string s = B.of_string s |> fun bytes -> {ok=true; bytes} in
    let to_string s = B.to_string s.bytes in
    let blit_bytes_to_buf ~src ~src_off ~src_len ~dst:dst0 ~dst_off =
      wf dst0;      
      let dst = dst0.bytes in
      let {off=src_pos} = src_off in
      let {len} = src_len in
      (* let dst = dst |> B.of_string in *)
      let {off=dst_pos} = dst_off in
      assert (
        src_pos + len <= B.length src |> fun b -> 
        b || (Printf.printf "%d %d %d\n" src_pos len (B.length src); false)
      );
      assert (
        dst_pos + len <= B.length dst |> fun b ->
        b || (Printf.printf "%d %d %d\n" dst_pos len (B.length dst); false)
      );
      B.blit ~src ~src_pos ~dst ~dst_pos ~len;
      dst0.ok <- false;
      {ok=true;bytes=dst}
    in
    let blit_string_to_buf ~src ~src_off ~src_len ~dst ~dst_off =
      blit_bytes_to_buf ~src:(Bytes.unsafe_of_string src) ~src_off ~src_len ~dst ~dst_off
    in
    let buf_size buf = buf.bytes |> B.length |> fun size -> {size} in
    {buf_create;buf_size;buf_to_string;of_string;to_string;blit_bytes_to_buf; blit_string_to_buf}

  let buf_ops = bytes_buf_ops
end

let bytes_buf_ops = Safe_bytes_buf.buf_ops


(* FIXME also want a bigstring version *)

(* FIXME rather than detecting reuse of an old buffer at runtime, we
   might model things explicitly by having "buffer descriptors", and
   all buffers are actually stored somewhere in the state. This is
   probably better from the semantics point of view. *)
