
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
  sums : int array array;
  slices : rect list;
}

type solution = rect list

(* Making states *)

let sum_stat { m = m1 ; t = t1 } { m = m2 ; t = t2 } =
  { m = m1 + m2 ; t = t1 + t2 }

let neg_stat { m ; t } = { m = -m ; t = -t }

let stat_of_cell = function M -> { m = 1 ; t = 0 } | T -> { m = 0 ; t = 1 }

let size_rects l h =
  let res = ref [] in
  for i = 1 to h do
    for j = 1 to h do
      if i * j <= h && i * j >= 2 * l then
        res := { tl = { r = 0; c = 0; }; br = { r = i - 1; c = j - 1; }; } :: !res
    done
  done;
  !res

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

let transpose t =
  Array.init (Array.length t.(0)) (fun i ->
      Array.init (Array.length t) (fun j -> t.(j).(i)))

let mk l h pizza =
  let pizza = transpose pizza in
  let sums = cumulated_sums_2d pizza
      (function M -> 1 | T -> 0) 0 ( + ) ( ~- ) in
  let slices = size_rects l h in
  { l; h; pizza; sums; slices; }

let area { tl = { r = a1; c =  b1}; br = { r = a2; c = b2 };} =
  (a2 - a1 + 1) * (b2 - b1 + 1)

let score sol =
  sol |> List.map area |> List.fold_left ( + ) 0

(* Some fn on points and rectangles *)

let add p p' = {
  r = p.r + p'.r;
  c = p.c + p'.c;
}

let width { tl; br; } = br.c - tl.c + 1
let height { tl; br; } = br.r - tl.r + 1

let shift p rect = { tl = add rect.tl p; br = add rect.br p; }


(* Eficient computing of sums *)

let sums_m {sums; _} { tl = { r = a1; c =  b1}; br = { r = a2; c = b2 };} = (* bounds included *)
  sums.(a2 + 1).(b2 + 1) - sums.(a2 + 1).(b1) - sums.(a1).(b2 + 1) + sums.(a1).(b1)

let sums_t st rect =
  area rect - sums_m st rect

(* Bound checking *)

let is_legal state rect =
  rect.tl.r >= 0 && rect.tl.c >= 0 &&
  rect.br.r < Array.length state.pizza &&
  rect.br.c < Array.length state.pizza.(0)

let is_valid state rect =
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

let pp_int fmt x = Format.fprintf fmt "%d " x

let pp_int_row fmt a =
  Array.iter (pp_int fmt) a;
  Format.pp_print_newline fmt ()

let pp_int_matrix fmt t =
  Array.iter (pp_int_row fmt) t;
  Format.pp_print_flush fmt ()

let pp_rect fmt { tl = { r = a1; c =  b1}; br = { r = a2; c = b2 };} =
  Format.fprintf fmt "%d %d %d %d" a1 b1 a2 b2;
  Format.pp_print_newline fmt ()

let pp_solution fmt l =
  Format.fprintf fmt "%d" (List.length l);
  Format.pp_print_newline fmt ();
  List.iter (pp_rect fmt) l;
  Format.pp_print_flush fmt ()
