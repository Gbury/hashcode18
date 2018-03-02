

let heuristic t ride time point =
  float_of_int (State.score t ride time point) /.
  float_of_int (State.finish_time t ride time point - time)

let heuristic2 t score ttime endpoint = float_of_int score /. float_of_int ttime

let max_pop = 15
let prof = 10
let take_prof = 5

let next t taken taken2 time point =
  Array.to_list t.State.rides |>
  CCList.filter_map (fun ride ->
      if Ride.is_reachable ride time point && not taken.(ride.Ride.id) && not (Ride.Set.mem ride taken2) then
        Some (ride, heuristic t ride time point)
      else
        None) |>
  List.sort (fun (_, a) (_, b) -> compare b a) |>
  CCList.take max_pop |>
  List.map fst

let next_pops t taken ls =
  ls |>
  List.map (fun ((score, ttime, endpoint, rides, nrides, taken2) as z) ->
      z ::
      (next t taken taken2 ttime endpoint |>
       List.map (fun ride ->
           (score + State.score t ride ttime endpoint,
            State.finish_time t ride ttime endpoint,
            ride.Ride.stop,
            ride :: rides,
            nrides + 1,
            Ride.Set.add ride taken2
           )
         ))
    ) |>
  List.flatten |>
  List.map (fun ((score, ttime, endpoint, _, n, _) as z) -> (z, (n, heuristic2 t score ttime endpoint))) |>
  List.sort (fun (_, a) (_, b) -> compare b a) |>
  CCList.take max_pop |>
  List.map fst

let iter_pops t taken time point =
  let rec aux n ls =
    if n = 0 then ls
    else aux (n - 1) (next_pops t taken ls)
  in aux prof [(0, time, point, [], 0, Ride.Set.empty)]

let best_pops t taken time point =
  let l = iter_pops t taken time point in
  match l with
  | [] -> []
  | (_, _, _, rides, _, _) :: _ -> CCList.take take_prof (List.rev rides)

let rec list_score t time point = function
  | [] -> 0
  | ride :: rest ->
    State.score t ride time point + list_score t (State.finish_time t ride time point) ride.Ride.stop rest

let rec list_end t time point = function
  | [] -> (time, point)
  | ride :: rest ->
    list_end t (State.finish_time t ride time point) ride.Ride.stop rest

let () = Random.init 42

let improve t sol =
  let taken = Array.make (Array.length t.State.rides) false in
  let cars = Array.init t.State.f (fun i -> (0, Point.mk 0 0, List.rev (Array.to_list sol.(i)))) in
  let score = ref 0 in
  let rec loop on =
    let change = ref false in
    Array.iter (fun i ->
        let time, point, rides = cars.(i) in
        let pp = best_pops t taken time point in
    if pp = [] then
      ()
    else
      (List.iter (fun ride -> taken.(ride.Ride.id) <- true) pp;
       let te, pe = list_end t time point pp in
       cars.(i) <- (te, pe, List.rev_append pp rides);
       score := !score + list_score t time point pp;
       change := true
       (* ;Format.eprintf "Score = %d@." !score *) )) on;
    if !change then loop on
  in
  Array.iter (fun x -> Array.iter (fun ride -> taken.(ride.Ride.id) <- true) x) sol;
  let climb () =
    let crs = Array.make (Array.length cars) false in
    for i = 1 to 20 do
      let n = Random.int (Array.length cars) in
      crs.(n) <- true
    done;
    let l = ref [] in
    Array.iteri (fun x b -> if b then l := x :: !l) crs;
    let old_cars = Array.copy cars in
    let old_score = !score in
    let old_taken = Array.copy taken in
    List.iter (fun i ->
        let (_, _, rides) = cars.(i) in
        List.iter (fun rd -> taken.(rd.Ride.id) <- false) rides;
        cars.(i) <- (0, Point.mk 0 0, []);
        score := !score - list_score t 0 (Point.mk 0 0) (List.rev rides);
      ) !l;
    loop (Array.of_list !l);
    Format.eprintf "Score = %d@." !score;
    if !score <= old_score then begin
      score := old_score;
      Array.iteri (fun i x -> cars.(i) <- x) old_cars;
      Array.iteri (fun i x -> taken.(i) <- x) old_taken;
      State.write_solution !score (Array.map (fun (_, _, rides) -> (Array.of_list (List.rev rides))) cars)
    end
  in
  for i = 1 to 100 do climb () done;
  Format.eprintf "Score = %d@." !score;
  Array.map (fun (_, _, rides) -> Array.of_list (List.rev rides)) cars



let solve t =
  let taken = Array.make (Array.length t.State.rides) false in
  let cars = Array.make t.State.f (0, Point.mk 0 0, []) in
  let score = ref 0 in
  let rec loop on =
    let change = ref false in
    Array.iter (fun i ->
        let time, point, rides = cars.(i) in
        let pp = best_pops t taken time point in
    if pp = [] then
      ()
    else
      (List.iter (fun ride -> taken.(ride.Ride.id) <- true) pp;
       let te, pe = list_end t time point pp in
       cars.(i) <- (te, pe, List.rev_append pp rides);
       score := !score + list_score t time point pp;
       change := true
       (* ;Format.eprintf "Score = %d@." !score *) )) on;
    if !change then loop on
  in
  (* loop (Array.init (Array.length cars) (fun i -> i)); *)
  for i = 0 to Array.length cars - 1 do loop [|i|]; Format.eprintf "tScore = %d@." !score; done;
  Format.eprintf "Score = %d@." !score;
  let climb () =
    let crs = Array.make (Array.length cars) false in
    for i = 1 to 20 do
      let n = Random.int (Array.length cars) in
      crs.(n) <- true
    done;
    let l = ref [] in
    Array.iteri (fun x b -> if b then l := x :: !l) crs;
    let old_cars = Array.copy cars in
    let old_score = !score in
    let old_taken = Array.copy taken in
    List.iter (fun i ->
        let (_, _, rides) = cars.(i) in
        List.iter (fun rd -> taken.(rd.Ride.id) <- false) rides;
        cars.(i) <- (0, Point.mk 0 0, []);
        score := !score - list_score t 0 (Point.mk 0 0) (List.rev rides);
      ) !l;
    (* loop (Array.of_list !l); *)
    List.iter (fun i -> loop [|i|]) !l;
    Format.eprintf "Score = %d@." !score;
    if !score <= old_score then begin
      score := old_score;
      Array.iteri (fun i x -> cars.(i) <- x) old_cars;
      Array.iteri (fun i x -> taken.(i) <- x) old_taken;
      State.write_solution !score (Array.map (fun (_, _, rides) -> (Array.of_list (List.rev rides))) cars)
    end
  in
  for i = 1 to 100 do climb () done;
  Format.eprintf "Score = %d@." !score;
  Array.map (fun (_, _, rides) -> Array.of_list (List.rev rides)) cars


