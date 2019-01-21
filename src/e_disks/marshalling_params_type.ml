(** A type for recording the marshalling functions *)

type ('k,'v,'page) marshalling_params = {
  frame_to_page: ('k,'v)Page_ref_int.frame -> 'page;
  page_to_frame: 'page -> ('k,'v)Page_ref_int.frame;
  page_size: int
}
