(** A type for managing the free space on the disk. *)
open Tjr_monad.Types

(** NOTE we assume alloc never fails, or that error is handled
   elsewhere in the monad *)
type ('blk_id,'t) blk_allocator_ops = {
  alloc: unit -> ('blk_id,'t) m; 
  free: 'blk_id -> (unit,'t) m;
}
  
