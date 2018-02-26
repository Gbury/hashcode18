
type cell = M | T

type stat = {
  m : int;
  t : int;
}

type point = { r : int; c : int; }

type rect = { tl : point; br : point; }

type t = {
  l : int;
  h : int;
  pizza : cell array array;
  sums : stat array array;
}

(* Making states *)

let sum_stat { m = m1 ; t = t1 } { m = m2 ; t = t2 } =
  { m = m1 + m2 ; t = t1 + t2 }

let neg_stat { m ; t } = { m = -m ; t = -t }

let stat_of_cell = function M -> { m = 1 ; t = 0 } | T -> { m = 0 ; t = 1 }

let cumulated_sums_2d data of_cell zero add neg =
  let n = Array.length data in
  let m = Array.length data.(0) in
  let sums = Array.make_matrix (n + 1) (m + 1) zero in
  for i = 1 to n do
    for j = 1 to m do
      sums.(i).(j) <- add
          (add (add sums.(i - 1).(j) sums.(i).(j - 1))
             (neg sums.(i - 1).(j - 1)))
          (of_cell data.(i - 1).(j - 1))
    done
  done;
  sums

let mk l h pizza =
  let sums = cumulated_sums_2d pizza
      stat_of_cell { m = 0; t = 0 } sum_stat neg_stat in
  { l; h; pizza; sums; }


(* Eficient computing of sums *)

let sums_m {sums; _} { tl = { r = a1; c =  b1}; br = { r = a2; c = b2 };} = (* bounds included *)
  sums.(a2 + 1).(b2 + 1).m - sums.(a2 + 1).(b1).m - sums.(a1).(b2 + 1).m + sums.(a1).(b1).m

let sums_t {sums; _} { tl = { r = a1; c =  b1}; br = { r = a2; c = b2 };} = (* bounds included *)
  sums.(a2 + 1).(b2 + 1).t - sums.(a2 + 1).(b1).t - sums.(a1).(b2 + 1).t + sums.(a1).(b1).t


(* Bound checking *)

let is_legal state rect =
  rect.tl.r >= 0 && rect.tl.c >= 0 &&
  rect.br.r < Array.length state.pizza &&
  rect.br.c < Array.length state.pizza.(0)

let is_valid_rect state rect =
  is_legal state rect &&
  sums_m state rect >= state.l &&
  sums_t state rect >= state.l


(* Some printing *)

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

