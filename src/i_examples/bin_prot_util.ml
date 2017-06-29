(* max sizes etc *)

open Small_string

let bin_size_int = Bin_prot.Size.Maximum.bin_size_int
let bin_size_ss = 3+SS.max_length

let bin_reader_ss = SS.bin_reader_ss
let bin_writer_ss = SS.bin_writer_ss

module BP = Bin_prot.Std

let bin_reader_int = BP.bin_reader_int
let bin_writer_int = BP.bin_writer_int

