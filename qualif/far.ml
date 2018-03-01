
type block = {
  length: int;
  x : int;
  y : int;
}

type stat = {
  block : block;
  starting : Ride.t list;
}

let left { length; x; _ } = length * x
let right { length; x; _ } = length * (x + 1) - 1
let bot { length; y; _ } = length * y
let top { length; y; _ } = length * (y + 1) - 1

type 'a blocks = {
  size : int;
  contents : 'a array array;
}

let is_in b Point.{ x; y; } =
  left b <= x && x <= right b && bot b <= y && y <= top b

let find_block blocks Point.{ x; y; } =
  { length = blocks.size; x = x / blocks.size; y = y / blocks.size; }

let add_ride blocks r =
  let p = r.Ride.start in
  let b = find_block blocks p in
  let s = blocks.contents.(b.x).(b.y) in
  blocks.contents.(b.x).(b.y) <- { s with starting = r :: s.starting; }

let rm_ride blocks r =
  let p = r.Ride.start in
  let b = find_block blocks p in
  let s = blocks.contents.(b.x).(b.y) in
  blocks.contents.(b.x).(b.y) <-
    { s with starting = CCList.remove ~eq:Ride.equal ~x:r s.starting; }

let iter blocks f =
  for i = 0 to Array.length blocks.contents - 1 do
    for j = 0 to Array.length blocks.contents.(0) - 1 do
      f i j
    done
  done

let tick blocks t =
  iter blocks (fun i j ->
      let s = blocks.contents.(i).(j) in
      blocks.contents.(i).(j) <-
        { s with starting = List.filter (fun r ->
              Ride.is_feasible r t) s.starting; }
    )

type car_config = {
  id : int;
  time : int;
  pos : Point.t;
  rides : Ride.t list;
}

type config = car_config array

let find_next_car a =
  let (i, _) = CCArray.foldi (fun (i, t) j cc ->
      if cc.time < t then (j, cc.time) else (i, t)) (-1, max_int) a in
  assert (i >= 0); i

let find_blocks seen blocks =
  let res = ref (-1,-1) in
  let max = ref min_int in
  iter blocks (fun i j ->
      if not (List.mem (i, j) seen) then ()
      else begin
        let s = blocks.contents.(i).(j) in
        let n = List.length s.starting in
        if n > !max then begin max := n; res := (i, j) end
      end
    );
  match !res with
  | (-1, -1) -> None
  | ((i, j) as ret) ->
    assert (i >= 0);
    assert (j >= 0);
    Some ret

let rec find_ride state blocks c seen =
  let (i, j) = find_block blocks c.pos in
  let s = blocks.contents.(i).(j) in
  let l = List.map (fun r ->
      let (x, y) = find_block blocks r.Ride.stop in
      let s = blocks.contents.(x).(y) in
      (r, List.length s.starting, x, y)) s.starting in
  let l' = List.sort (fun (_, n, _, _) (_, n', _, _) ->


  match find_blocks seen blocks with
  | None -> None
  | Some (i, j) ->
    let b = blocks.contents.(i).(j) in
    let l = List.filter (fun r ->
        Ride.is_reachable r c.time c.pos) b.starting in
    let l' = List.map (fun r -> (r, State.score state r c.time c.pos)) l in
    let l'' = List.sort (fun (_, s) (_, s') -> s' - s) l' in
    begin match l'' with
      | [] -> find_ride state blocks c ((i, j) :: seen)
      | (r, _) :: _ -> Some r
    end

let rec solve state blocks config =
  let i = find_next_car config in
  let c = config.(i) in
  tick blocks c.time;
  match find_ride state blocks c [] with
  | None ->
    config.(i) <- { c with 


let far_block state l =
  let stat_empty = { starting = []; } in
  let blocks = {
    size = l; contents =
                Array.init (state.State.r / l) (fun _ ->
                    Array.make (state.State.c / l) stat_empty); } in
  Array.iter (add_ride blocks) state.State.rides;
  ()


