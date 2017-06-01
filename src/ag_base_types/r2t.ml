open Tree

(** The r2t type represents a pure view of a given system state. It is
   constructed as a function from system state. The result is a
   function which takes a reference r and constructs the (key,value)
   tree rooted at r. In case the state is not wellformed, the return
   type is actually a tree option. NOTE this function takes the global
   state t, not some particular store state. *)
type ('k,'v,'r,'t) r2t = ('t -> 'r -> ('k,'v) tree option)
