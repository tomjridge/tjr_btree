module Fun :
  sig val id : 'a -> 'a val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b end
module Orderings :
  sig
    type 'a ord = { less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool; }
    val less_eq : 'a ord -> 'a -> 'a -> bool
    val less : 'a ord -> 'a -> 'a -> bool
    val max : 'a ord -> 'a -> 'a -> 'a
  end
module Arith :
  sig
    type nat
    val less_eq_nat : nat -> nat -> bool
    val less_nat : nat -> nat -> bool
    val ord_nat : nat Orderings.ord
    type int = Int_of_integer of Big_int.big_int
    type num = One | Bit0 of num | Bit1 of num
    val plus_nat : nat -> nat -> nat
    val one_nat : nat
    val suc : nat -> nat
    val less_int : int -> int -> bool
    val zero_int : int
    val zero_nat : nat
    val nat_of_integer : Big_int.big_int -> nat
    val equal_int : int -> int -> bool
    val less_eq_int : int -> int -> bool
    val equal_nat : nat -> nat -> bool
    val minus_nat : nat -> nat -> nat
    val times_nat : nat -> nat -> nat
  end
module List :
  sig
    val nth : 'a list -> Arith.nat -> 'a
    val upt : Arith.nat -> Arith.nat -> Arith.nat list
    val zip : 'a list -> 'b list -> ('a * 'b) list
    val drop : Arith.nat -> 'a list -> 'a list
    val find : ('a -> bool) -> 'a list -> 'a option
    val null : 'a list -> bool
    val last : 'a list -> 'a
    val take : Arith.nat -> 'a list -> 'a list
    val foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    val concat : 'a list list -> 'a list
    val filter : ('a -> bool) -> 'a list -> 'a list
    val butlast : 'a list -> 'a list
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val list_ex : ('a -> bool) -> 'a list -> bool
    val map : ('a -> 'b) -> 'a list -> 'b list
    val pred_list : ('a -> bool) -> 'a list -> bool
    val size_list : 'a list -> Arith.nat
  end
module Set :
  sig
    type 'a set = Set of 'a list | Coset of 'a list
    val ball : 'a set -> ('a -> bool) -> bool
  end
module Product_Type : sig val fst : 'a * 'b -> 'a val snd : 'a * 'b -> 'b end
module Prelude :
  sig
    type min_size_t = Small_root_node_or_leaf | Small_node | Small_leaf
    type 'a constants_ext =
        Constants_ext of Arith.nat * Arith.nat * Arith.nat * Arith.nat * 'a
    val max_leaf_size : 'a constants_ext -> Arith.nat
    val max_node_keys : 'a constants_ext -> Arith.nat
    val min_leaf_size : 'a constants_ext -> Arith.nat
    val min_node_keys : 'a constants_ext -> Arith.nat
  end
module String :
  sig
    type nibble =
        Nibble0
      | Nibble1
      | Nibble2
      | Nibble3
      | Nibble4
      | Nibble5
      | Nibble6
      | Nibble7
      | Nibble8
      | Nibble9
      | NibbleA
      | NibbleB
      | NibbleC
      | NibbleD
      | NibbleE
      | NibbleF
  end
module Option : sig val is_none : 'a option -> bool end
module Res :
  sig
    external raise : exn -> 'a = "%raise"
    external raise_notrace : exn -> 'a = "%raise_notrace"
    val invalid_arg : string -> 'a
    val failwith : string -> 'a
    exception Exit
    external ( = ) : 'a -> 'a -> bool = "%equal"
    external ( <> ) : 'a -> 'a -> bool = "%notequal"
    external ( < ) : 'a -> 'a -> bool = "%lessthan"
    external ( > ) : 'a -> 'a -> bool = "%greaterthan"
    external ( <= ) : 'a -> 'a -> bool = "%lessequal"
    external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
    external compare : 'a -> 'a -> int = "%compare"
    val min : 'a -> 'a -> 'a
    val max : 'a -> 'a -> 'a
    external ( == ) : 'a -> 'a -> bool = "%eq"
    external ( != ) : 'a -> 'a -> bool = "%noteq"
    external not : bool -> bool = "%boolnot"
    external ( && ) : bool -> bool -> bool = "%sequand"
    external ( & ) : bool -> bool -> bool = "%sequand"
    external ( || ) : bool -> bool -> bool = "%sequor"
    external ( or ) : bool -> bool -> bool = "%sequor"
    external __LOC__ : string = "%loc_LOC"
    external __FILE__ : string = "%loc_FILE"
    external __LINE__ : int = "%loc_LINE"
    external __MODULE__ : string = "%loc_MODULE"
    external __POS__ : string * int * int * int = "%loc_POS"
    external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
    external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
    external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
    external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
    external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
    external ( ~- ) : int -> int = "%negint"
    external ( ~+ ) : int -> int = "%identity"
    external succ : int -> int = "%succint"
    external pred : int -> int = "%predint"
    external ( + ) : int -> int -> int = "%addint"
    external ( - ) : int -> int -> int = "%subint"
    external ( * ) : int -> int -> int = "%mulint"
    external ( / ) : int -> int -> int = "%divint"
    external ( mod ) : int -> int -> int = "%modint"
    val abs : int -> int
    val max_int : int
    val min_int : int
    external ( land ) : int -> int -> int = "%andint"
    external ( lor ) : int -> int -> int = "%orint"
    external ( lxor ) : int -> int -> int = "%xorint"
    val lnot : int -> int
    external ( lsl ) : int -> int -> int = "%lslint"
    external ( lsr ) : int -> int -> int = "%lsrint"
    external ( asr ) : int -> int -> int = "%asrint"
    external ( ~-. ) : float -> float = "%negfloat"
    external ( ~+. ) : float -> float = "%identity"
    external ( +. ) : float -> float -> float = "%addfloat"
    external ( -. ) : float -> float -> float = "%subfloat"
    external ( *. ) : float -> float -> float = "%mulfloat"
    external ( /. ) : float -> float -> float = "%divfloat"
    external ( ** ) : float -> float -> float = "caml_power_float" "pow"
      [@@unboxed] [@@noalloc]
    external sqrt : float -> float = "caml_sqrt_float" "sqrt" [@@unboxed]
      [@@noalloc]
    external exp : float -> float = "caml_exp_float" "exp" [@@unboxed]
      [@@noalloc]
    external log : float -> float = "caml_log_float" "log" [@@unboxed]
      [@@noalloc]
    external log10 : float -> float = "caml_log10_float" "log10" [@@unboxed]
      [@@noalloc]
    external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
      [@@unboxed] [@@noalloc]
    external log1p : float -> float = "caml_log1p_float" "caml_log1p"
      [@@unboxed] [@@noalloc]
    external cos : float -> float = "caml_cos_float" "cos" [@@unboxed]
      [@@noalloc]
    external sin : float -> float = "caml_sin_float" "sin" [@@unboxed]
      [@@noalloc]
    external tan : float -> float = "caml_tan_float" "tan" [@@unboxed]
      [@@noalloc]
    external acos : float -> float = "caml_acos_float" "acos" [@@unboxed]
      [@@noalloc]
    external asin : float -> float = "caml_asin_float" "asin" [@@unboxed]
      [@@noalloc]
    external atan : float -> float = "caml_atan_float" "atan" [@@unboxed]
      [@@noalloc]
    external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
      [@@unboxed] [@@noalloc]
    external hypot : float -> float -> float = "caml_hypot_float"
      "caml_hypot" [@@unboxed] [@@noalloc]
    external cosh : float -> float = "caml_cosh_float" "cosh" [@@unboxed]
      [@@noalloc]
    external sinh : float -> float = "caml_sinh_float" "sinh" [@@unboxed]
      [@@noalloc]
    external tanh : float -> float = "caml_tanh_float" "tanh" [@@unboxed]
      [@@noalloc]
    external ceil : float -> float = "caml_ceil_float" "ceil" [@@unboxed]
      [@@noalloc]
    external floor : float -> float = "caml_floor_float" "floor" [@@unboxed]
      [@@noalloc]
    external abs_float : float -> float = "%absfloat"
    external copysign : float -> float -> float = "caml_copysign_float"
      "caml_copysign" [@@unboxed] [@@noalloc]
    external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
      [@@unboxed] [@@noalloc]
    external frexp : float -> float * int = "caml_frexp_float"
    external ldexp :
      (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed])
      = "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
    external modf : float -> float * float = "caml_modf_float"
    external float : int -> float = "%floatofint"
    external float_of_int : int -> float = "%floatofint"
    external truncate : float -> int = "%intoffloat"
    external int_of_float : float -> int = "%intoffloat"
    val infinity : float
    val neg_infinity : float
    val nan : float
    val max_float : float
    val min_float : float
    val epsilon_float : float
    type fpclass =
      fpclass =
        FP_normal
      | FP_subnormal
      | FP_zero
      | FP_infinite
      | FP_nan
    external classify_float : (float [@unboxed]) -> fpclass
      = "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]
    val ( ^ ) : string -> string -> string
    external int_of_char : char -> int = "%identity"
    val char_of_int : int -> char
    external ignore : 'a -> unit = "%ignore"
    val string_of_bool : bool -> string
    val bool_of_string : string -> bool
    val string_of_int : int -> string
    external int_of_string : string -> int = "caml_int_of_string"
    val string_of_float : float -> string
    external float_of_string : string -> float = "caml_float_of_string"
    external fst : 'a * 'b -> 'a = "%field0"
    external snd : 'a * 'b -> 'b = "%field1"
    val ( @ ) : 'a list -> 'a list -> 'a list
    type in_channel = in_channel
    type out_channel = out_channel
    val stdin : in_channel
    val stdout : out_channel
    val stderr : out_channel
    val print_char : char -> unit
    val print_string : string -> unit
    val print_bytes : bytes -> unit
    val print_int : int -> unit
    val print_float : float -> unit
    val print_endline : string -> unit
    val print_newline : unit -> unit
    val prerr_char : char -> unit
    val prerr_string : string -> unit
    val prerr_bytes : bytes -> unit
    val prerr_int : int -> unit
    val prerr_float : float -> unit
    val prerr_endline : string -> unit
    val prerr_newline : unit -> unit
    val read_line : unit -> string
    val read_int : unit -> int
    val read_float : unit -> float
    type open_flag =
      open_flag =
        Open_rdonly
      | Open_wronly
      | Open_append
      | Open_creat
      | Open_trunc
      | Open_excl
      | Open_binary
      | Open_text
      | Open_nonblock
    val open_out : string -> out_channel
    val open_out_bin : string -> out_channel
    val open_out_gen : open_flag list -> int -> string -> out_channel
    val flush : out_channel -> unit
    val flush_all : unit -> unit
    val output_char : out_channel -> char -> unit
    val output_string : out_channel -> string -> unit
    val output_bytes : out_channel -> bytes -> unit
    val output : out_channel -> bytes -> int -> int -> unit
    val output_substring : out_channel -> string -> int -> int -> unit
    val output_byte : out_channel -> int -> unit
    val output_binary_int : out_channel -> int -> unit
    val output_value : out_channel -> 'a -> unit
    val seek_out : out_channel -> int -> unit
    val pos_out : out_channel -> int
    val out_channel_length : out_channel -> int
    val close_out : out_channel -> unit
    val close_out_noerr : out_channel -> unit
    val set_binary_mode_out : out_channel -> bool -> unit
    val open_in : string -> in_channel
    val open_in_bin : string -> in_channel
    val open_in_gen : open_flag list -> int -> string -> in_channel
    val input_char : in_channel -> char
    val input_line : in_channel -> string
    val input : in_channel -> bytes -> int -> int -> int
    val really_input : in_channel -> bytes -> int -> int -> unit
    val really_input_string : in_channel -> int -> string
    val input_byte : in_channel -> int
    val input_binary_int : in_channel -> int
    val input_value : in_channel -> 'a
    val seek_in : in_channel -> int -> unit
    val pos_in : in_channel -> int
    val in_channel_length : in_channel -> int
    val close_in : in_channel -> unit
    val close_in_noerr : in_channel -> unit
    val set_binary_mode_in : in_channel -> bool -> unit
    module LargeFile = LargeFile
    type 'a ref = 'a ref = { mutable contents : 'a; }
    external ref : 'a -> 'a ref = "%makemutable"
    external ( ! ) : 'a ref -> 'a = "%field0"
    external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
    external incr : int ref -> unit = "%incr"
    external decr : int ref -> unit = "%decr"
    type ('a, 'b) result = ('a, 'b) result = Ok of 'a | Error of 'b
    type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
        ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6
    type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
    type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
    val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
    external format_of_string :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
      = "%identity"
    val ( ^^ ) :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      ('f, 'b, 'c, 'e, 'g, 'h) format6 -> ('a, 'b, 'c, 'd, 'g, 'h) format6
    val exit : int -> 'a
    val at_exit : (unit -> unit) -> unit
    val valid_float_lexem : string -> string
    val unsafe_really_input : in_channel -> bytes -> int -> int -> unit
    val do_at_exit : unit -> unit
    type 'a res = ('a, string) result
  end
module Util :
  sig
    external raise : exn -> 'a = "%raise"
    external raise_notrace : exn -> 'a = "%raise_notrace"
    val invalid_arg : string -> 'a
    val failwith : string -> 'a
    exception Exit
    external ( = ) : 'a -> 'a -> bool = "%equal"
    external ( <> ) : 'a -> 'a -> bool = "%notequal"
    external ( < ) : 'a -> 'a -> bool = "%lessthan"
    external ( > ) : 'a -> 'a -> bool = "%greaterthan"
    external ( <= ) : 'a -> 'a -> bool = "%lessequal"
    external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
    external compare : 'a -> 'a -> int = "%compare"
    val min : 'a -> 'a -> 'a
    val max : 'a -> 'a -> 'a
    external ( == ) : 'a -> 'a -> bool = "%eq"
    external ( != ) : 'a -> 'a -> bool = "%noteq"
    external not : bool -> bool = "%boolnot"
    external ( && ) : bool -> bool -> bool = "%sequand"
    external ( & ) : bool -> bool -> bool = "%sequand"
    external ( || ) : bool -> bool -> bool = "%sequor"
    external ( or ) : bool -> bool -> bool = "%sequor"
    external __LOC__ : string = "%loc_LOC"
    external __FILE__ : string = "%loc_FILE"
    external __LINE__ : int = "%loc_LINE"
    external __MODULE__ : string = "%loc_MODULE"
    external __POS__ : string * int * int * int = "%loc_POS"
    external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
    external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
    external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
    external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
    external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
    external ( ~- ) : int -> int = "%negint"
    external ( ~+ ) : int -> int = "%identity"
    external succ : int -> int = "%succint"
    external pred : int -> int = "%predint"
    external ( + ) : int -> int -> int = "%addint"
    external ( - ) : int -> int -> int = "%subint"
    external ( * ) : int -> int -> int = "%mulint"
    external ( / ) : int -> int -> int = "%divint"
    external ( mod ) : int -> int -> int = "%modint"
    val abs : int -> int
    val max_int : int
    val min_int : int
    external ( land ) : int -> int -> int = "%andint"
    external ( lor ) : int -> int -> int = "%orint"
    external ( lxor ) : int -> int -> int = "%xorint"
    val lnot : int -> int
    external ( lsl ) : int -> int -> int = "%lslint"
    external ( lsr ) : int -> int -> int = "%lsrint"
    external ( asr ) : int -> int -> int = "%asrint"
    external ( ~-. ) : float -> float = "%negfloat"
    external ( ~+. ) : float -> float = "%identity"
    external ( +. ) : float -> float -> float = "%addfloat"
    external ( -. ) : float -> float -> float = "%subfloat"
    external ( *. ) : float -> float -> float = "%mulfloat"
    external ( /. ) : float -> float -> float = "%divfloat"
    external ( ** ) : float -> float -> float = "caml_power_float" "pow"
      [@@unboxed] [@@noalloc]
    external sqrt : float -> float = "caml_sqrt_float" "sqrt" [@@unboxed]
      [@@noalloc]
    external exp : float -> float = "caml_exp_float" "exp" [@@unboxed]
      [@@noalloc]
    external log : float -> float = "caml_log_float" "log" [@@unboxed]
      [@@noalloc]
    external log10 : float -> float = "caml_log10_float" "log10" [@@unboxed]
      [@@noalloc]
    external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
      [@@unboxed] [@@noalloc]
    external log1p : float -> float = "caml_log1p_float" "caml_log1p"
      [@@unboxed] [@@noalloc]
    external cos : float -> float = "caml_cos_float" "cos" [@@unboxed]
      [@@noalloc]
    external sin : float -> float = "caml_sin_float" "sin" [@@unboxed]
      [@@noalloc]
    external tan : float -> float = "caml_tan_float" "tan" [@@unboxed]
      [@@noalloc]
    external acos : float -> float = "caml_acos_float" "acos" [@@unboxed]
      [@@noalloc]
    external asin : float -> float = "caml_asin_float" "asin" [@@unboxed]
      [@@noalloc]
    external atan : float -> float = "caml_atan_float" "atan" [@@unboxed]
      [@@noalloc]
    external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
      [@@unboxed] [@@noalloc]
    external hypot : float -> float -> float = "caml_hypot_float"
      "caml_hypot" [@@unboxed] [@@noalloc]
    external cosh : float -> float = "caml_cosh_float" "cosh" [@@unboxed]
      [@@noalloc]
    external sinh : float -> float = "caml_sinh_float" "sinh" [@@unboxed]
      [@@noalloc]
    external tanh : float -> float = "caml_tanh_float" "tanh" [@@unboxed]
      [@@noalloc]
    external ceil : float -> float = "caml_ceil_float" "ceil" [@@unboxed]
      [@@noalloc]
    external floor : float -> float = "caml_floor_float" "floor" [@@unboxed]
      [@@noalloc]
    external abs_float : float -> float = "%absfloat"
    external copysign : float -> float -> float = "caml_copysign_float"
      "caml_copysign" [@@unboxed] [@@noalloc]
    external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
      [@@unboxed] [@@noalloc]
    external frexp : float -> float * int = "caml_frexp_float"
    external ldexp :
      (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed])
      = "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
    external modf : float -> float * float = "caml_modf_float"
    external float : int -> float = "%floatofint"
    external float_of_int : int -> float = "%floatofint"
    external truncate : float -> int = "%intoffloat"
    external int_of_float : float -> int = "%intoffloat"
    val infinity : float
    val neg_infinity : float
    val nan : float
    val max_float : float
    val min_float : float
    val epsilon_float : float
    type fpclass =
      fpclass =
        FP_normal
      | FP_subnormal
      | FP_zero
      | FP_infinite
      | FP_nan
    external classify_float : (float [@unboxed]) -> fpclass
      = "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]
    val ( ^ ) : string -> string -> string
    external int_of_char : char -> int = "%identity"
    val char_of_int : int -> char
    external ignore : 'a -> unit = "%ignore"
    val string_of_bool : bool -> string
    val bool_of_string : string -> bool
    val string_of_int : int -> string
    external int_of_string : string -> int = "caml_int_of_string"
    val string_of_float : float -> string
    external float_of_string : string -> float = "caml_float_of_string"
    external fst : 'a * 'b -> 'a = "%field0"
    external snd : 'a * 'b -> 'b = "%field1"
    val ( @ ) : 'a list -> 'a list -> 'a list
    type in_channel = in_channel
    type out_channel = out_channel
    val stdin : in_channel
    val stdout : out_channel
    val stderr : out_channel
    val print_char : char -> unit
    val print_string : string -> unit
    val print_bytes : bytes -> unit
    val print_int : int -> unit
    val print_float : float -> unit
    val print_endline : string -> unit
    val print_newline : unit -> unit
    val prerr_char : char -> unit
    val prerr_string : string -> unit
    val prerr_bytes : bytes -> unit
    val prerr_int : int -> unit
    val prerr_float : float -> unit
    val prerr_endline : string -> unit
    val prerr_newline : unit -> unit
    val read_line : unit -> string
    val read_int : unit -> int
    val read_float : unit -> float
    type open_flag =
      open_flag =
        Open_rdonly
      | Open_wronly
      | Open_append
      | Open_creat
      | Open_trunc
      | Open_excl
      | Open_binary
      | Open_text
      | Open_nonblock
    val open_out : string -> out_channel
    val open_out_bin : string -> out_channel
    val open_out_gen : open_flag list -> int -> string -> out_channel
    val flush : out_channel -> unit
    val flush_all : unit -> unit
    val output_char : out_channel -> char -> unit
    val output_string : out_channel -> string -> unit
    val output_bytes : out_channel -> bytes -> unit
    val output : out_channel -> bytes -> int -> int -> unit
    val output_substring : out_channel -> string -> int -> int -> unit
    val output_byte : out_channel -> int -> unit
    val output_binary_int : out_channel -> int -> unit
    val output_value : out_channel -> 'a -> unit
    val seek_out : out_channel -> int -> unit
    val pos_out : out_channel -> int
    val out_channel_length : out_channel -> int
    val close_out : out_channel -> unit
    val close_out_noerr : out_channel -> unit
    val set_binary_mode_out : out_channel -> bool -> unit
    val open_in : string -> in_channel
    val open_in_bin : string -> in_channel
    val open_in_gen : open_flag list -> int -> string -> in_channel
    val input_char : in_channel -> char
    val input_line : in_channel -> string
    val input : in_channel -> bytes -> int -> int -> int
    val really_input : in_channel -> bytes -> int -> int -> unit
    val really_input_string : in_channel -> int -> string
    val input_byte : in_channel -> int
    val input_binary_int : in_channel -> int
    val input_value : in_channel -> 'a
    val seek_in : in_channel -> int -> unit
    val pos_in : in_channel -> int
    val in_channel_length : in_channel -> int
    val close_in : in_channel -> unit
    val close_in_noerr : in_channel -> unit
    val set_binary_mode_in : in_channel -> bool -> unit
    module LargeFile :
      sig
        val seek_out : out_channel -> int64 -> unit
        val pos_out : out_channel -> int64
        val out_channel_length : out_channel -> int64
        val seek_in : in_channel -> int64 -> unit
        val pos_in : in_channel -> int64
        val in_channel_length : in_channel -> int64
      end
    type 'a ref = 'a ref = { mutable contents : 'a; }
    external ref : 'a -> 'a ref = "%makemutable"
    external ( ! ) : 'a ref -> 'a = "%field0"
    external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
    external incr : int ref -> unit = "%incr"
    external decr : int ref -> unit = "%decr"
    type ('a, 'b) result = ('a, 'b) result = Ok of 'a | Error of 'b
    type ('a, 'b, 'c, 'd, 'e, 'f) format6 =
        ('a, 'b, 'c, 'd, 'e, 'f) CamlinternalFormatBasics.format6
    type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
    type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
    val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
    external format_of_string :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
      = "%identity"
    val ( ^^ ) :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      ('f, 'b, 'c, 'e, 'g, 'h) format6 -> ('a, 'b, 'c, 'd, 'g, 'h) format6
    val exit : int -> 'a
    val at_exit : (unit -> unit) -> unit
    val valid_float_lexem : string -> string
    val unsafe_really_input : in_channel -> bytes -> int -> int -> unit
    val do_at_exit : unit -> unit
    type 'a res = ('a, string) result
    type error = String_error of string
    val rev_apply : 'a -> ('a -> 'b) -> 'b
    val unzip : ('a * 'b) list -> 'a list * 'b list
    val from_to : Arith.nat -> Arith.nat -> Arith.nat list
    val is_None : 'a option -> bool
    val failwitha : string -> 'a
    val split_at : Arith.nat -> 'a list -> 'a list * 'a list
    val dest_Some : 'a option -> 'a
    val dest_list : 'a list -> 'a * 'a list
    val iter_step : ('a -> 'a option) -> 'a -> 'a
    val dest_lista : 'a list -> 'a list * 'a
    val split_at_3 : Arith.nat -> 'a list -> 'a list * ('a * 'a list)
    val assert_true : 'a -> bool -> bool
    val impossible1 : string -> 'a
    val max_of_list : Arith.nat list -> Arith.nat
    val assert_truea : bool -> bool
  end
module Key_value :
  sig
    val key_eq : ('a -> 'a -> Arith.int) -> 'a -> 'a -> bool
    val key_lt : ('a -> 'a -> Arith.int) -> 'a -> 'a -> bool
    val kvs_equal : ('a * 'b) list -> ('a * 'b) list -> bool
    val check_keys :
      ('a -> 'a -> Arith.int) -> 'a option -> 'a Set.set -> 'a option -> bool
    val kvs_delete :
      ('a -> 'a -> Arith.int) -> 'a -> ('a * 'b) list -> ('a * 'b) list
    val kvs_insert :
      ('a -> 'a -> Arith.int) -> 'a * 'b -> ('a * 'b) list -> ('a * 'b) list
    val split_leaf :
      unit Prelude.constants_ext ->
      ('a * 'b) list -> ('a * 'b) list * ('a * ('a * 'b) list)
    val split_node :
      unit Prelude.constants_ext ->
      'a list * 'b list -> ('a list * 'b list) * ('a * ('a list * 'b list))
    val split_ks_rs :
      ('a -> 'a -> Arith.int) ->
      'a ->
      'a list * 'b list -> ('a list * 'b list) * ('b * ('a list * 'b list))
    val ordered_key_list : ('a -> 'a -> Arith.int) -> 'a list -> bool
  end
module Tree :
  sig
    type ('a, 'b) tree =
        Node of ('a list * ('a, 'b) tree list)
      | Leaf of ('a * 'b) list
    val height : ('a, 'b) tree -> Arith.nat
    val dest_Node : ('a, 'b) tree -> 'a list * ('a, 'b) tree list
    val tree_equal : ('a, 'b) tree -> ('a, 'b) tree -> bool
    val tree_to_leaves : ('a, 'b) tree -> ('a * 'b) list list
    val tree_to_kvs : ('a, 'b) tree -> ('a * 'b) list
    val tree_to_keys : ('a, 'b) tree -> 'a Set.set
    val wellformed_tree :
      unit Prelude.constants_ext ->
      Prelude.min_size_t option ->
      ('a -> 'a -> Arith.int) -> ('a, 'b) tree -> bool
  end
module Tree_stack :
  sig
    type ('a, 'b, 'c) ts_frame_ext =
        Ts_frame_ext of 'a list * 'b list * 'b * 'a list * 'b list * 'c
    val stack_map :
      ('a -> 'b) ->
      ('c, 'a, unit) ts_frame_ext list -> ('c, 'b, unit) ts_frame_ext list
    val no_focus :
      ('a, 'b, unit) ts_frame_ext list ->
      ('a, 'b option, unit) ts_frame_ext list
    val r_stk_to_rs : ('a, 'b, unit) ts_frame_ext list -> 'b list
    val stack_equal :
      ('a, 'b, unit) ts_frame_ext list ->
      ('a, 'b, unit) ts_frame_ext list -> bool
    val dest_ts_frame :
      ('a, 'b, unit) ts_frame_ext ->
      ('a list * 'b list) * ('b * ('a list * 'b list))
    val tree_to_stack :
      ('a -> 'a -> Arith.int) ->
      'a ->
      ('a, 'b) Tree.tree ->
      Arith.nat ->
      ('a, 'b) Tree.tree * ('a, ('a, 'b) Tree.tree, unit) ts_frame_ext list
    val stack_to_lu_of_child :
      ('a, 'b, unit) ts_frame_ext list -> 'a option * 'a option
    val add_new_stack_frame :
      ('a -> 'a -> Arith.int) ->
      'a ->
      'a list * 'b list ->
      ('a, 'b, unit) ts_frame_ext list ->
      ('a, 'b, unit) ts_frame_ext list * 'b
  end
module Frame :
  sig
    type ('a, 'b, 'c) frame =
        Node_frame of ('a list * 'c list)
      | Leaf_frame of ('a * 'b) list
    val dest_Leaf_frame : ('a, 'b, 'c) frame -> ('a * 'b) list
    val dest_Node_frame : ('a, 'b, 'c) frame -> 'a list * 'c list
  end
module Pre_params :
  sig
    val dummy : unit
    val mk_r2t :
      ('a -> ('b, 'c, 'a) Frame.frame option) ->
      Arith.nat -> 'a -> ('b, 'c) Tree.tree option
  end
module Params :
  sig
    type 'a ps0 =
        Ps0 of (('a -> 'a -> Arith.int) * unit Prelude.constants_ext)
    type ('a, 'b, 'c, 'd) ps1 =
        Ps1 of
          ('a ps0 *
           (('c -> 'd -> 'd * ('a, 'b, 'c) Frame.frame Util.res) *
            ((('a, 'b, 'c) Frame.frame -> 'd -> 'd * 'c Util.res) *
             ('c list -> 'd -> 'd * unit Util.res))))
    val ps0 : ('a, 'b, 'c, 'd) ps1 -> 'a ps0
    val csa : 'a ps0 -> unit Prelude.constants_ext
    val cs : ('a, 'b, 'c, 'd) ps1 -> unit Prelude.constants_ext
    val cmp_ka : 'a ps0 -> 'a -> 'a -> Arith.int
    val cmp_k : ('a, 'b, 'c, 'd) ps1 -> 'a -> 'a -> Arith.int
    val dummy : unit
    val store_free :
      ('a, 'b, 'c, 'd) ps1 -> 'c list -> 'd -> 'd * unit Util.res
    val store_read :
      ('a, 'b, 'c, 'd) ps1 ->
      'c -> 'd -> 'd * ('a, 'b, 'c) Frame.frame Util.res
    val store_alloc :
      ('a, 'b, 'c, 'd) ps1 ->
      ('a, 'b, 'c) Frame.frame -> 'd -> 'd * 'c Util.res
  end
module Monad :
  sig
    val bind :
      ('a -> 'b -> 'b * 'c Util.res) ->
      ('b -> 'b * 'a Util.res) -> 'b -> 'b * 'c Util.res
    val fmap :
      ('a -> 'b) -> ('c -> 'c * 'a Util.res) -> 'c -> 'c * 'b Util.res
    val return : 'a -> 'b -> 'b * 'a Util.res
  end
module Find :
  sig
    type ('a, 'b, 'c) find_state
    val find_step :
      ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) find_state -> 'd -> 'd * ('a, 'b, 'c) find_state Util.res
    val mk_find_state : 'a -> 'b -> ('a, 'c, 'b) find_state
    val wf_store_tree :
      ('a -> ('b, 'c, 'a) Frame.frame option) ->
      'a -> ('b, 'c) Tree.tree -> bool
    val dest_f_finished :
      ('a, 'b, 'c) find_state ->
      ('c *
       ('a *
        ('c * (('a * 'b) list * ('a, 'c, unit) Tree_stack.ts_frame_ext list))))
      option
    val wellformed_find_state :
      ('a -> 'a -> Arith.int) ->
      ('b -> ('a, 'c, 'b) Frame.frame option) ->
      ('a, 'c) Tree.tree -> ('a, 'c, 'b) find_state -> bool
  end
module Delete :
  sig
    type ('a, 'b, 'c) del_t =
        D_small_leaf of ('a * 'b) list
      | D_small_node of ('a list * 'c list)
      | D_updated_subtree of 'c
    type ('a, 'b, 'c) delete_state =
        D_down of (('a, 'b, 'c) Find.find_state * 'c)
      | D_up of
          (('a, 'b, 'c) del_t *
           (('a, 'c, unit) Tree_stack.ts_frame_ext list * 'c))
      | D_finished of 'c
    val delete_step :
      ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) delete_state ->
      'd -> 'd * ('a, 'b, 'c) delete_state Util.res
    val dest_d_finished : ('a, 'b, 'c) delete_state -> 'c option
    val mk_delete_state : 'a -> 'b -> ('a, 'c, 'b) delete_state
    val wellformed_delete_state :
      'a Params.ps0 ->
      ('b -> ('a, 'c, 'b) Frame.frame option) ->
      ('a, 'c) Tree.tree -> 'a -> ('a, 'c, 'b) delete_state -> bool
  end
module Insert :
  sig
    type ('a, 'b, 'c) i12_t = I1 of 'c | I2 of ('c * ('a * 'c))
    type ('a, 'b, 'c) insert_state =
        I_down of (('a, 'b, 'c) Find.find_state * 'b)
      | I_up of
          (('a, 'b, 'c) i12_t * ('a, 'c, unit) Tree_stack.ts_frame_ext list)
      | I_finished of 'c
    val insert_step :
      ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) insert_state ->
      'd -> 'd * ('a, 'b, 'c) insert_state Util.res
    val dest_i_finished : ('a, 'b, 'c) insert_state -> 'c option
    val mk_insert_state : 'a -> 'b -> 'c -> ('a, 'b, 'c) insert_state
    val wellformed_insert_state :
      'a Params.ps0 ->
      ('b -> ('a, 'c, 'b) Frame.frame option) ->
      ('a, 'c) Tree.tree -> 'a -> 'c -> ('a, 'c, 'b) insert_state -> bool
  end
module Insert_many :
  sig
    type ('a, 'b, 'c) fo =
        I1 of ('c * ('a * 'b) list)
      | I2 of (('c * ('a * 'c)) * ('a * 'b) list)
    type ('a, 'b, 'c) ist =
        I_down of (('a, 'b, 'c) Find.find_state * ('b * ('a * 'b) list))
      | I_up of
          (('a, 'b, 'c) fo * ('a, 'c, unit) Tree_stack.ts_frame_ext list)
      | I_finished of ('c * ('a * 'b) list)
    val insert_step :
      ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) ist -> 'd -> 'd * ('a, 'b, 'c) ist Util.res
    val dest_i_finished : ('a, 'b, 'c) ist -> ('c * ('a * 'b) list) option
    val mk_insert_state :
      'a -> 'b -> ('a * 'b) list -> 'c -> ('a, 'b, 'c) ist
  end
module Leaf_stream :
  sig
    type ('a, 'b, 'c) ls_state
    val lss_step :
      ('a, 'b, 'c, 'd) Params.ps1 ->
      ('a, 'b, 'c) ls_state -> 'd -> 'd * ('a, 'b, 'c) ls_state Util.res
    val mk_ls_state : 'a -> ('b, 'c, 'a) ls_state
    val dest_LS_leaf : ('a, 'b, 'c) ls_state -> ('a * 'b) list option
    val lss_is_finished : ('a, 'b, 'c) ls_state -> bool
  end
