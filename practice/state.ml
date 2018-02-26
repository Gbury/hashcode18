
type cell = M | T

type stat = {
  m : int;
  t : int;
}

let add_m s = { s with m = s.m + 1; }
let add_t s = { s with t = s.t + 1; }
let add s = function M -> add_m s | T -> add_t s

let sum_stat { m = m1 ; t = t1 } { m = m2 ; t = t2 } =
  { m = m1 + m2 ; t = t1 + t2 }

let neg_stat { m ; t } = { m = -m ; t = -t }

type t = {
  l : int;
  h : int;
  pizza : cell array array;
  sums : stat array array;
}

let mk l h pizza =
  let n = Array.length pizza in
  let m = Array.length pizza.(0) in
  let sums = Array.make_matrix (n + 1) (m + 1) {m = 0; t= 0} in
  for i = 1 to n do
    for j = 1 to m do
      sums.(i).(j) <- add
          (sum_stat (sum_stat sums.(i - 1).(j) sums.(i).(j - 1))
             (neg_stat sums.(i - 1).(j - 1)))
          pizza.(i - 1).(j - 1)
    done
  done;
  { l; h; pizza; sums; }

let sums_m sums (a1, b1) (a2, b2) = (* bounds included *)
  sums.(a2 + 1).(b2 + 1).m - sums.(a2 + 1).(b1).m - sums.(a1).(b2 + 1).m + sums.(a1).(b1).m

let sums_t sums (a1, b1) (a2, b2) = (* bounds included *)
  sums.(a2 + 1).(b2 + 1).t - sums.(a2 + 1).(b1).t - sums.(a1).(b2 + 1).t + sums.(a1).(b1).t

let string_of_cell = function M -> "M" | T -> "T"
let pp_pizza_cell fmt c = Format.fprintf fmt "%s" (string_of_cell c)

let pp_pizza_row fmt a =
  Array.iter (pp_pizza_cell fmt) a;
  Format.pp_print_newline fmt ()

let pp_pizza fmt t =
  Array.iter (pp_pizza_row fmt) t;
  Format.pp_print_flush fmt ()


let pp_stat fmt { m; t; } = Format.fprintf fmt "(%2d,%2d)" m t

let pp_stat_row fmt a =
  Array.iter (pp_stat fmt) a;
  Format.pp_print_newline fmt ()

let pp_stat_matrix fmt t =
  Array.iter (pp_stat_row fmt) t;
  Format.pp_print_flush fmt ()

