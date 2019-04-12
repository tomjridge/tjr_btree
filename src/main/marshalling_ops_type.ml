(** A type for recording the marshalling functions *)

type ('dnode,'blk) marshalling_ops = {
  dnode_to_blk: 'dnode -> 'blk;
  blk_to_dnode: 'blk -> 'dnode;
  marshal_blk_size: int
}
