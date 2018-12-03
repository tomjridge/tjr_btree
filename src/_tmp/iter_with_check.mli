module IU = Isa_util
type ('t, 'f) iter_ops = {
  check_state : 't -> bool;
  check_trans : 't -> 't -> bool;
  step : 't -> 't * (unit, string) result;
  dest : 't -> 'f option;
}
val iter : ('t, 'f) iter_ops -> 't -> 't * ('f, string) result
val if_some : ('a -> unit) -> 'a option -> unit
val map_option : ('a -> 'b) -> 'a option -> 'b option
module Find :
  sig
    type ('k, 'v, 'r, 't) t = {
      tree : ('k, 'v) Tree.tree option;
      r2f : ('t -> ('k, 'v, 'r) IU.r2f) option;
      store : 't;
      fs : ('k, 'v, 'r) IU.find_state;
      ps1 : ('k, 'v, 'r, 't) IU.Params_.ps1;
    }
    type ('k, 'v, 'r) finished = 'r * ('k * 'v) list
  end
val find_ops : (('a, 'b, 'c, 'd) Find.t, ('a, 'b, 'c) Find.finished) iter_ops
val find :
  ('k, 'v, 'r, 't) IU.Params_.ps1 ->
  ('t -> ('k, 'v, 'r) IU.r2f) option ->
  ('k, 'v) Tree.tree option ->
  'k -> 'r -> 't -> 't * ('r * ('k * 'v) list, string) result
module Insert :
  sig
    type ('k, 'v, 'r, 't) t = {
      tree : ('k, 'v) Tree.tree option;
      r2f : ('t -> ('k, 'v, 'r) IU.r2f) option;
      k : 'k;
      v : 'v;
      store : 't;
      is : ('k, 'v, 'r) IU.insert_state;
      ps1 : ('k, 'v, 'r, 't) IU.Params_.ps1;
    }
    type 'r finished = 'r
  end
val insert_ops :
  (('a, 'b, 'c Insert.finished, 'd) Insert.t, 'c Insert.finished) iter_ops
val insert :
  ('a, 'b, 'c Insert.finished, 'd) IU.Params_.ps1 ->
  ('d -> ('a, 'b, 'c Insert.finished) IU.r2f) option ->
  ('a, 'b) Tree.tree option ->
  'a ->
  'b -> 'c Insert.finished -> 'd -> 'd * ('c Insert.finished, string) result
module Im :
  sig
    type ('k, 'v, 'r, 't) t = {
      tree : ('k, 'v) Tree.tree option;
      r2f : ('t -> ('k, 'v, 'r) IU.r2f) option;
      k : 'k;
      v : 'v;
      kvs : ('k * 'v) list;
      store : 't;
      is : ('k, 'v, 'r) IU.im_state;
      ps1 : ('k, 'v, 'r, 't) IU.Params_.ps1;
    }
    type 'r finished = 'r
  end
val im_ops :
  (('a, 'b, 'c, 'd) Im.t, ('c * ('a * 'b) list) Im.finished) iter_ops
val insert_many :
  ('a, 'b, 'c, 'd) IU.Params_.ps1 ->
  ('d -> ('a, 'b, 'c) IU.r2f) option ->
  ('a, 'b) Tree.tree option ->
  'a ->
  'b ->
  ('a * 'b) list ->
  'c -> 'd -> 'd * (('c * ('a * 'b) list) Im.finished, string) result
module Delete :
  sig
    type ('k, 'v, 'r, 't) t = {
      tree : ('k, 'v) Tree.tree option;
      r2f : ('t -> ('k, 'v, 'r) IU.r2f) option;
      k : 'k;
      store : 't;
      ds : ('k, 'v, 'r) IU.delete_state;
      ps1 : ('k, 'v, 'r, 't) IU.Params_.ps1;
    }
    type 'r finished = 'r
  end
val check_state : ('a, 'b, 'c, 'd) Delete.t -> bool
val check_trans : 'a -> 'b -> bool
val dest :
  ('a, 'b, 'r Delete.finished, 'c) Delete.t -> 'r Delete.finished option
val step :
  ('a, 'b, 'c, 'd) Delete.t ->
  ('a, 'b, 'c, 'd) Delete.t * (unit, string) result
val delete_ops :
  (('a, 'b, 'c Delete.finished, 'd) Delete.t, 'c Delete.finished) iter_ops
val mk :
  ('a, 'b, 'c, 'd) IU.Params_.ps1 ->
  ('d -> ('a, 'b, 'c) IU.r2f) option ->
  ('a, 'b) Tree.tree option -> 'a -> 'c -> 'd -> ('a, 'b, 'c, 'd) Delete.t
val delete :
  ('a, 'b, 'c Delete.finished, 't) IU.Params_.ps1 ->
  ('t -> ('a, 'b, 'c Delete.finished) IU.r2f) option ->
  ('a, 'b) Tree.tree option ->
  'a -> 'c Delete.finished -> 't -> 't * ('c Delete.finished, string) result
