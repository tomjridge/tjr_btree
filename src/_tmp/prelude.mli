module Mut = State_error_monad.Mut
module Sem = State_error_monad.Sem
module State_error_monad = State_error_monad.State_error_monad
module type MONAD =
  sig
    type 'a m
    val bind : ('a -> 'b m) -> 'a m -> 'b m
    val return : 'a -> 'a m
  end
module Util :
  sig
    val flush_out : unit -> unit
    val read_file : string -> string
    val iter_step : ('s -> 's option) -> 's -> 's
  end
module Int = Pervasives_.Int
module Set_int = Pervasives_.Set_int
module Map_int = Pervasives_.Map_int
module Map_string = Pervasives_.Map_string
val impossible : string -> 'a
val dest_Some : 'a option -> 'a
val option_map : ('a -> 'b) -> 'a option -> 'b option
val flush_out : unit -> unit
val fd_from_file : fn:string -> create:bool -> init:bool -> Unix.file_descr
module Pickle_params :
  sig
    type ('k, 'v) t = {
      p_k : 'k -> Pickle.P.m;
      u_k : 'k Pickle.U.m;
      k_len : int;
      p_v : 'v -> Pickle.P.m;
      u_v : 'v Pickle.U.m;
      v_len : int;
    }
  end
