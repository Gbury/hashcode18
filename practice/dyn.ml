
type index = State.point array

type config = {
  index: index;
  score : int;
  solution : State.rect list;
}

let possible_rect_list state p =
  let l = List.map (State.shift p) state.State.slices in
  List.filter (State.is_valid state) l

