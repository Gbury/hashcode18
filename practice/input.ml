
let read_int () = Scanf.scanf " %d " (fun x -> x)

let read_rect () =
  let a1 = read_int () in
  let a2 = read_int () in
  let a3 = read_int () in
  let a4 = read_int () in
  { State.tl = { State.r = a1 ; c = a2 };
    br = { State.r=a3 ; c = a4 } }

let read_cell i j = Scanf.scanf " %c " (function
    | 'T' -> State.T
    | 'M' -> State.M
    | c -> Format.eprintf "read '%c' at %d, %d@." c i j; assert false)

let read_state () =
  let r = read_int () in
  let c = read_int () in
  let l = read_int () in
  let h = read_int () in
  let pizza = Array.init r (fun i ->
      Array.init c (fun j -> read_cell i j)) in
  State.mk l h pizza

let read_solution () =
  let s = read_int () in
  let l = ref [] in
    for i=1 to s do
      l := read_rect ()::!l
    done;
  !l



