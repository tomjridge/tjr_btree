module IE = Isa_export
type 'a res = 'a IE.Util.res
type ('a, 't) m = 't -> 't * 'a IE.Util.res
type ('k, 'r) rstk = ('k, 'r, unit) IE.Tree_stack.ts_frame_ext list
type ('k, 'v, 'r) r2f = 'r -> ('k, 'v, 'r) IE.Frame.frame option
type ('k, 'v, 'r) find_state = ('k, 'v, 'r) IE.Find.find_state
type ('k, 'v, 'r) insert_state = ('k, 'v, 'r) IE.Insert.insert_state
type ('k, 'v, 'r) im_state = ('k, 'v, 'r) IE.Insert_many.ist
type ('k, 'v, 'r) delete_state = ('k, 'v, 'r) IE.Delete.delete_state
type ('k, 'v, 'r) ls_state = ('k, 'v, 'r) IE.Leaf_stream.ls_state
module Params_ :
  sig
    type 'k ps0 = { compare_k : 'k -> 'k -> int; constants : Constants.t; }
    type ('k, 'v, 'r, 't) ps1 = {
      ps0 : 'k ps0;
      store_read : 'r -> 't -> 't * ('k, 'v, 'r) IE.Frame.frame res;
      store_free : 'r list -> 't -> 't * unit res;
      store_alloc : ('k, 'v, 'r) IE.Frame.frame -> 't -> 't * 'r res;
    }
  end
module X :
  sig
    val int_to_nat : int -> Isa_export.Arith.nat
    val int_to_int : int -> Isa_export.Arith.int
    val x_constants : Constants.t -> unit IE.Prelude.constants_ext
    val x_cmp : ('a -> 'b -> int) -> 'a -> 'b -> Isa_export.Arith.int
    val x_ps0 : 'k Params_.ps0 -> 'k IE.Params.ps0
    val x_ps1 :
      ('k, 'v, 'r, 't) Params_.ps1 -> ('k, 'v, 'r, 't) IE.Params.ps1
  end
val x5 : 'a * ('b * ('c * ('d * 'e))) -> 'a * 'b * 'c * 'd * 'e
val mk_find_state : 'k -> 'r -> ('k, 'v, 'r) find_state
val find_step :
  ('k, 'v, 'r, 't) Params_.ps1 ->
  ('k, 'v, 'r) find_state -> 't -> 't * ('k, 'v, 'r) find_state res
val dest_f_finished :
  ('k, 'v, 'r) find_state ->
  ('r * 'k * 'r * ('k * 'v) list * ('k, 'r) rstk) option
val wellformed_find_state :
  'k Params_.ps0 ->
  ('k, 'v, 'r) r2f ->
  ('k, 'v) IE.Tree.tree -> ('k, 'v, 'r) find_state -> bool
val mk_delete_state : 'k -> 'r -> ('k, 'v, 'r) delete_state
val delete_step :
  ('k, 'v, 'r, 't) Params_.ps1 ->
  ('k, 'v, 'r) delete_state -> 't -> 't * ('k, 'v, 'r) delete_state res
val dest_d_finished : ('k, 'v, 'r) delete_state -> 'r option
val wellformed_delete_state :
  'k Params_.ps0 ->
  ('k, 'v, 'r) r2f ->
  ('k, 'v) IE.Tree.tree -> 'k -> ('k, 'v, 'r) delete_state -> bool
val mk_insert_state : 'k -> 'v -> 'r -> ('k, 'v, 'r) insert_state
val insert_step :
  ('k, 'v, 'r, 't) Params_.ps1 ->
  ('k, 'v, 'r) insert_state -> 't -> 't * ('k, 'v, 'r) insert_state res
val dest_i_finished : ('k, 'v, 'r) insert_state -> 'r option
val wellformed_insert_state :
  'k Params_.ps0 ->
  ('k, 'v, 'r) r2f ->
  ('k, 'v) IE.Tree.tree -> 'k -> 'v -> ('k, 'v, 'r) insert_state -> bool
val mk_im_state : 'k -> 'v -> ('k * 'v) list -> 'r -> ('k, 'v, 'r) im_state
val im_step :
  ('k, 'v, 'r, 't) Params_.ps1 ->
  ('k, 'v, 'r) im_state -> 't -> 't * ('k, 'v, 'r) im_state res
val dest_im_finished : ('k, 'v, 'r) im_state -> ('r * ('k * 'v) list) option
val mk_ls_state : 'r -> ('k, 'v, 'r) ls_state
val ls_step :
  ('k, 'v, 'r, 't) Params_.ps1 ->
  ('k, 'v, 'r) IE.Leaf_stream.ls_state ->
  't -> 't * ('k, 'v, 'r) ls_state res
val ls_dest_leaf :
  ('a, 'b, 'c) IE.Leaf_stream.ls_state -> ('a * 'b) list option
val ls_is_finished : ('a, 'b, 'c) IE.Leaf_stream.ls_state -> bool
