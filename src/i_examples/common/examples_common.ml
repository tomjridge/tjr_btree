open Map_on_fd
open Map_on_fd.Default_implementation

module Internal = struct
  module P = Params.P
  type nonrec ('a,'b,'c,'d) t = 
< close : Map_on_fd.Default_implementation.t -> unit;
  disk_ops : Map_on_fd.Default_implementation.t Tjr_monad.State_passing.state_passing Disk_ops.block_device;
  from_file : fn:string ->
              create:bool -> init:bool -> Map_on_fd.Default_implementation.t;
  imperative_map_ops : s_ref:t ref ->
                       [ `Imperative_map_ops of
                            ('a -> 'b option) * ('a -> 'b -> unit) *
                            ('a -> unit) ];
  ls_ops : ('a, 'b, int, t Tjr_monad.State_passing.state_passing)
           Leaf_stream_ops.leaf_stream_ops;
  map_ops : ('a, 'b, t Tjr_monad.State_passing.state_passing) Map_ops.map_ops;
  store_ops : ('a, 'b, int, t Tjr_monad.State_passing.state_passing)
              Store_ops.store_ops >
end

open Internal

(* FIXME this is specific to fd - rename this file? *)
(** Construct various api layers based on parameters ps *)
let mk_example_on_fd ~ps : ('a,'b,'c,'d) Internal.t = 
  let constants = P.constants ps in
  let cmp = P.cmp ps in
  let disk_ops = mk_disk_ops ~monad_ops ~ps ~fd_ops in
  let store_ops = 
    Disk_to_store.disk_to_store 
      ~monad_ops ~ps ~disk_ops ~free_ops in
  let map_ops = 
    Store_to_map.store_ops_to_map_ops 
      ~monad_ops ~constants ~cmp ~page_ref_ops ~store_ops 
  in
  let imperative_map_ops = 
    Map_ops.state_passing_map_ops_to_imperative map_ops 
  in
  let ls_ops = 
    Store_to_map.store_ops_to_ls_ops ~monad_ops ~constants ~cmp ~store_ops 
  in
  let from_file ~fn ~create ~init = 
    Map_on_fd.Default_implementation.from_file ~fn ~create ~init ~ps 
  in
  let close = Map_on_fd.Default_implementation.close ~blk_sz:(P.blk_sz ps) in
  object
    method disk_ops=disk_ops
    method store_ops=store_ops
    method map_ops=map_ops
    method imperative_map_ops=imperative_map_ops
    method ls_ops=ls_ops
    method from_file=from_file
    method close=close
  end

let _ = mk_example_on_fd


(* specific params ------------------------------------------------ *)

module P = struct
  (* FIXME move elsewhere? add types? *)
  let map_ops (x:('a,'b,'c,'d)Internal.t) = x#map_ops
  let imperative_map_ops (x:('a,'b,'c,'d)Internal.t) = x#imperative_map_ops
  let ls_ops (x:('a,'b,'c,'d)Internal.t) = x#ls_ops
  let from_file (x:('a,'b,'c,'d)Internal.t) = x#from_file
  let close (x:('a,'b,'c,'d)Internal.t) = x#close
end

(* default blocksize ------------------------------------------------ *)

let blk_sz = 4096


