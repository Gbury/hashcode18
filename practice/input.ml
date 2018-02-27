
let read_int ic = Scanf.bscanf ic " %d " (fun x -> x)

let read_rect ic =
  let a1 = read_int ic in
  let a2 = read_int ic in
  let a3 = read_int ic in
  let a4 = read_int ic in
  { State.tl = { State.r = a1 ; c = a2 };
    br = { State.r=a3 ; c = a4 } }

let read_cell ic i j =
  Scanf.bscanf ic " %c " (function
    | 'T' -> State.T
    | 'M' -> State.M
    | c -> Format.eprintf "read '%c' at %d, %d@." c i j; assert false)

let read_state ic =
  let r = read_int ic in
  let c = read_int ic in
  let l = read_int ic in
  let h = read_int ic in
  let pizza = Array.init r (fun i ->
      Array.init c (fun j -> read_cell ic i j)) in
  State.mk l h pizza

let read_solution ic =
  let s = read_int ic in
  let l = ref [] in
    for i=1 to s do
      l := read_rect ic :: !l
    done;
  !l



