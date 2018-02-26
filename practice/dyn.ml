
type index = int array

type config = {
  score : int;
  line : int;
  index : index;
  solution : State.rect list;
}

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
  let new_c = rect.State.br.c in
  let i = rect.State.tl.r - line in
  let j = i + State.height rect in
  for k = i to j do
    res.(k) <- new_c
  done;
  res

let add_rect config rect =
  if not (valid_rect config rect) then raise Exit
  else begin
    let line = config.line in
    let score = State.(height rect * width rect) + config.score in
    let index = add_rect_idx config.line config.index rect in
    { score; line; index; solution = rect :: config.solution; }
  end

