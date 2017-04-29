open Pickle
type ('k,'v) t = {
  p_k: 'k -> P.m;
  u_k: 'k U.m;
  k_len: int;
  p_v: 'v -> P.m;
  u_v: 'v U.m;
  v_len: int      
}
