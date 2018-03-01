
type block = {
  length: int;
  x : int;
  y : int;
}

type stat = {
  block : block;
  malus : int;
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

let take_ride blocks r =
  let p = r.Ride.stop in
  let b = find_block blocks p in
  let s = blocks.contents.(b.x).(b.y) in
  blocks.contents.(b.x).(b.y) <- { s with malus = s.malus + 1; }

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
    { s with malus = s.malus - 1;
             starting = CCList.remove ~eq:Ride.equal ~x:r s.starting; }

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
        let n = List.length s.starting - s.malus in
        if n > !max then begin max := n; res := (i, j) end
      end
    );
  match !res with
  | (-1, -1) -> None
  | ((i, j) as ret) ->
    assert (i >= 0);
    assert (j >= 0);
    Some ret

let center b =
  Point.mk ((left b + right b) / 2) ((top b + bot b) / 2)

let dist pos b =
  Point.dist pos (center b)

let closest_blocks blocks c =
  let all_blocks = CCList.flat_map Array.to_list @@ Array.to_list blocks.contents in
  let l = List.map (fun s -> (s.block, dist c.pos s.block)) all_blocks in
  let l' = List.sort (fun (_, n) (_, n') -> n - n') l in
  List.map fst l'

let rec find_ride_aux state blocks c b =
  let s = blocks.contents.(b.x).(b.y) in
  let l = List.filter (fun r -> Ride.is_reachable r c.time c.pos) s.starting in
  let l' = List.map (fun r ->
      let b' = find_block blocks r.Ride.stop in
      let s = blocks.contents.(b'.x).(b'.y) in
      (r, List.length s.starting, b'.x, b'.y)) l in
  let l'' = List.sort (fun (_, n, _, _) (_, n', _, _) -> n' - n) l' in
  match l'' with
  | (r, _, i, j) :: _ ->
    Format.eprintf "found nice ride@.";
    Some r
  | [] ->
    Format.eprintf "looking for closest block@.";
    CCList.find_map (find_ride_aux state blocks c) (closest_blocks blocks c)

let rec find_ride state blocks c =
  let b = find_block blocks c.pos in
  find_ride_aux state blocks c b

let rec solve state blocks config =
  let i = find_next_car config in
  let c = config.(i) in
  tick blocks c.time;
  match find_ride state blocks c with
  | Some r ->
    rm_ride blocks r;
    take_ride blocks r;
    let time = State.finish_time state r c.time c.pos in
    let pos = r.Ride.stop in
    config.(i) <- { c with time; pos; rides = r :: c.rides; };
    solve state blocks config
  | None ->
    if c.time >= state.State.t then ()
    else begin
      config.(i) <- { c with time = state.State.t };
      solve state blocks config
    end

let far_block state l =
  let blocks = {
    size = l; contents =
                Array.init (state.State.r / l + 1) (fun x ->
                    Array.init (state.State.c / l + 1) (fun y ->
                        { block = { length = l; x; y; }; starting = []; malus = 0; }
                      ));
  } in
  Array.iter (add_ride blocks) state.State.rides;
  let config = Array.init state.State.f (fun i ->
      { id = i; time = 0; pos = Point.mk 0 0; rides = []; }) in
  let () = solve state blocks config in
  Array.map (fun c -> Array.of_list @@ List.rev c.rides) config

let pp fmt a =
  Array.iter (fun a ->
      let n = Array.length a in
      Format.fprintf fmt "%d " n;
      Array.iter (fun r ->
          Format.fprintf fmt "%d " r.Ride.id
        ) a;
      Format.fprintf fmt "@.") a

