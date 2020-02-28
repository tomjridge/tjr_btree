(** Flags:

- O_TRUNC, reinitialize B-tree root to point to an empty leaf; possibly also truncate the underlying file
- O_NOCACHE, do not use a write back cache (the underlying file may still need to be sync'ed tho)
*)
type flg = O_TRUNC | O_NOCACHE






