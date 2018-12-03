external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : bytes -> int -> char -> unit = "%string_safe_set"
external create : int -> bytes = "caml_create_string"
val make : int -> char -> string
val init : int -> (int -> char) -> string
val copy : string -> string
val sub : string -> int -> int -> string
val fill : bytes -> int -> int -> char -> unit
val blit : string -> int -> bytes -> int -> int -> unit
val concat : string -> string list -> string
val iter : (char -> unit) -> string -> unit
val iteri : (int -> char -> unit) -> string -> unit
val map : (char -> char) -> string -> string
val mapi : (int -> char -> char) -> string -> string
val trim : string -> string
val escaped : string -> string
val index : string -> char -> int
val rindex : string -> char -> int
val index_from : string -> int -> char -> int
val rindex_from : string -> int -> char -> int
val contains_from : string -> int -> char -> bool
val rcontains_from : string -> int -> char -> bool
val uppercase : string -> string
val lowercase : string -> string
val capitalize : string -> string
val uncapitalize : string -> string
val uppercase_ascii : string -> string
val lowercase_ascii : string -> string
val capitalize_ascii : string -> string
val uncapitalize_ascii : string -> string
type t = string
val compare : t -> t -> int
val equal : t -> t -> bool
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string" [@@noalloc]
external unsafe_fill : bytes -> int -> int -> char -> unit
  = "caml_fill_string" [@@noalloc]
module X_list : sig val last : 'a list -> 'a end
module Span :
  sig
    type t = { s : string; i : int; j : int; }
    val of_string : string -> t
    val to_string : t -> string
    val length : t -> int
    val pred_to_indexes : (t -> int list) -> t -> int list
    val equal : t -> t -> bool
    val starts : prefix:t -> t -> bool
  end
val indexes : sub:string -> string -> int list
val starts : prefix:string -> string -> bool
val ends : suffix:string -> string -> bool
val contains : sub:string -> string -> bool
val split_at : string -> int -> string * string
val replace_first : sub:string -> rep:string -> string -> string
val replace_last : sub:string -> rep:string -> string -> string
val replace_all : sub:string -> rep:string -> string -> string
val drop : int -> string -> string
val split_on_first : sub:string -> string -> string * string
val split_on_last : sub:string -> string -> string * string
val split_on_all : sub:string -> string -> string list
val concat_strings : sep:string -> string list -> string
val replace_list : string -> (string * string) list -> string
val exp : string -> char list
val imp : char list -> string
