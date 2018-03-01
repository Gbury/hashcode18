
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
  Format.eprintf "Adding ride %d to block (%d, %d)@." r.Ride.id b.x b.y;
  blocks.contents.(b.x).(b.y) <- { s with starting = r :: s.starting; }

let rm_ride blocks r =
  let p = r.Ride.start in
  let b = find_block blocks p in
  let s = blocks.contents.(b.x).(b.y) in
  Format.eprintf "Removing ride %d from block (%d, %d)@." r.Ride.id b.x b.y;
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
      let l, l' = List.partition (fun r ->
          Ride.is_feasible r t) s.starting in
      if l' <> [] then
        Format.eprintf "filtering rides %a from block (%d, %d)@."
          CCFormat.(list ~sep:(return "@ ") Ride.pp_id) l' i j;
      blocks.contents.(i).(j) <- { s with starting = l; }
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

let closest_blocks blocks pos =
  let all_blocks = CCList.flat_map Array.to_list @@ Array.to_list blocks.contents in
  let l = CCList.map (fun s -> (s.block, dist pos s.block)) all_blocks in
  let l' = CCList.sort (fun (_, n) (_, n') -> n - n') l in
  CCList.map fst l'

let best_ride state blocks i j time pos =
  let s = blocks.contents.(i).(j) in
  let l = List.filter (fun r -> Ride.is_reachable r time pos) s.starting in
  let l' = List.map (fun r -> (r, State.score state r time pos)) l in
  let l'' = List.sort (fun (_, n) (_, n') -> n' - n) l' in
  match l'' with
  | (r, _) :: _ -> Some r
  | [] -> None

let c_score n d = n * d

let rec find_ride_aux state blocks c b =
  let s = blocks.contents.(b.x).(b.y) in
  let l = List.filter (fun r -> Ride.is_reachable r c.time c.pos) s.starting in
  let l' = List.map (fun r ->
      let b' = find_block blocks r.Ride.stop in
      let l = CCList.take 5 (closest_blocks (center b')) in
      let l' = List.map (fun b'' -> List.length blocks.contents.(b''.x).(b''.y).starting) l in
      let score = c_score (List.length s.starting) (dist c.pos b') in
      (r, score, b'.x, b'.y)) l in
  let l'' = List.sort (fun (_, n, _, _) (_, n', _, _) -> n' - n) l' in
  match l'' with
  | (r, n, _, _) :: _ ->
    let l''' = CCList.filter_map (fun (r, n', x, y) ->
        if n = n' then Some (r, State.score state r c.time c.pos, x, y) else None) l'' in
    let l4 = List.sort (fun (_, n, _, _) (_, n', _, _) -> n' - n) l''' in
    begin match l4 with
      | (r, _, i, j) :: _ ->
        Format.eprintf "found nice ride@.";
        let time = State.finish_time state r c.time c.pos in
        begin match best_ride state blocks i j time r.Ride.stop with
          | None -> [r]
          | Some r' ->
            if not (Ride.equal r r') then [r; r'] else [r]
        end
      | [] -> assert false
    end
  | [] -> []

let rec find f = function
  | [] -> []
  | x :: r ->
    begin match f x with
      | [] -> find f r
      | res -> res
    end

let rec find_ride state blocks c =
  let b = find_block blocks c.pos in
  match find_ride_aux state blocks c b with
  | [] ->
    Format.eprintf "looking for closest block@.";
    find (find_ride_aux state blocks c) (closest_blocks blocks c)
  | res -> res

let perform state blocks i c r =
  rm_ride blocks r;
  take_ride blocks r;
  let time = State.finish_time state r c.time c.pos in
  Format.eprintf "assigned ride %a to car %d, finish at %d@." Ride.pp_id r i time;
  let pos = r.Ride.stop in
  { c with time; pos; rides = r :: c.rides; }

let rec solve state blocks config =
  let i = find_next_car config in
  let c = config.(i) in
  Format.eprintf "looking at car %d with time %d@." i c.time;
  tick blocks c.time;
  match find_ride state blocks c with
  | [] ->
    if Array.for_all (fun c -> c.time >= state.State.t) config then ()
    else begin
      Format.eprintf "sending car %d to hell@." i;
      config.(i) <- { c with time = state.State.t };
      solve state blocks config
    end
  | l ->
    let c' = List.fold_left (perform state blocks i) c l in
    config.(i) <- c';
    solve state blocks config

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

