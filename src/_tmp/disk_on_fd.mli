type fd = Unix.file_descr
type 't fd_ops = {
  get_fd : unit -> (fd, 't) Btree_api.m;
  set_fd : fd -> (unit, 't) Btree_api.m;
}
val safely : string -> ('a, 't) Btree_api.m -> ('a, 't) Btree_api.m
val make_disk : Btree_api.BLK.sz -> 't fd_ops -> 't Btree_api.disk_ops
