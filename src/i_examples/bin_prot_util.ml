(** Binprot util: max sizes etc *)


(* int -------------------------------------------------------------- *)

open Bin_prot.Std

let bp_size_int = Bin_prot.Size.Maximum.bin_size_int

let bin_reader_int = bin_reader_int

let bin_writer_int = bin_writer_int



(* small string ----------------------------------------------------- *)

open Tjr_fs_shared.Small_string

let bp_size_ss = 3+max_length

let bin_reader_ss = bin_reader_ss

let bin_writer_ss = bin_writer_ss

