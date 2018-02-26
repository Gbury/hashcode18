
type index = int array

type config = {
  score : int;
  line : int;
  index : index;
  solution : State.rect list;
}

let pp_index fmt a =
  Array.iter (fun x -> Format.fprintf fmt "%d@ " x) a

let pp fmt config =
  Format.fprintf fmt "@[<hv>(%d) %d -@ %a@ @[<hov>%a@]@]"
    config.score config.line pp_index config.index State.pp_solution config.solution

let possible_rect_list state p =
  let l = List.map (State.shift p) state.State.slices in
  List.filter (State.is_valid state) l

let valid_left_border index i h x =
  i + h - 1 < Array.length index &&
  begin try
    for k = i to i + h - 1 do
      if index.(k) > x then raise Exit
    done;
    true
  with Exit ->
    false
end

let valid_rect config rect =
  let open State in
  rect.tl.r >= config.line &&
  valid_left_border config.index
    (rect.tl.r - config.line) (height rect) rect.tl.c

let add_rect_idx line index rect =
  let res = Array.copy index in
  let new_c = rect.State.br.c + 1 in
  let i = rect.State.tl.r - line in
  let j = i + State.height rect - 1 in
  for k = i to j do
    res.(k) <- new_c
  done;
  res

let add_rect config rect =
  if not (valid_rect config rect) then raise Exit
  else begin
    let line = config.line in
    let score = State.area rect + config.score in
    let index = add_rect_idx config.line config.index rect in
    { score; line; index; solution = rect :: config.solution; }
  end

type st = index list array * (index, config) Hashtbl.t

let add_config (h, q) config =
  let index = config.index in
  match Hashtbl.find q index with
  | config2 ->
    if config.score > config2.score then
      Hashtbl.replace q index config
  | exception Not_found -> begin
      let k = Array.fold_left (+) 0 index in
      h.(k) <- index :: h.(k);
      Hashtbl.add q index config
    end

let inf = 1_000_000_000
let valid_rects state config =
  let mn = Array.fold_left min inf config.index in
  let cols = Array.length (state.State.pizza.(0)) in
  let result = ref [] in
  Array.iteri (fun i col ->
      if col = mn then begin
        let p = State.{ r = config.line + i ; c = col } in
        result := (List.map (add_rect config) (List.filter (valid_rect config) (possible_rect_list state p))) @ !result;
        if col < cols then begin
          let nindex = Array.copy config.index in
          nindex.(i) <- col + 1;
          result := { config with index = nindex } :: !result
        end
    end
    ) config.index;
  !result

let process_config state st config =
  List.iter (add_config st) (valid_rects state config)

let dyn state line slice_length =
  let index = Array.make slice_length 0 in
  let config = { score = 0 ; line = line ; index = index ; solution = [] } in
  let cols = Array.length (state.State.pizza.(0)) in
  let h = Array.make (cols * slice_length + 1) [] in
  let q = Hashtbl.create 65537 in
  Hashtbl.add q index config;
  h.(0) <- [index];
  for i = 0 to cols * slice_length do
    List.iter (fun index -> process_config state (h, q) (Hashtbl.find q index)) h.(i)
  done;
  try
    Hashtbl.find q (Array.make slice_length cols)
  with
    Not_found -> { score = 0 ; line = line ; index = Array.make slice_length cols ; solution = [] }

let split_and_conquer ~j state f k =
  let n = Array.length state.State.pizza in
  let l = CCList.init (n / k + 1) (fun x -> x) in
  let res = Parmap.parmap (fun i ->
      Format.eprintf "starting line %d@." (i * k);
      let res = (f state (i * k) k).solution in
      Format.eprintf "finished line %d@." (i * k);
      res
    ) (Parmap.L l) in
  List.flatten res

let solve ~state ~j ~n = split_and_conquer ~j state dyn n

