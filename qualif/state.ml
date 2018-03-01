
type t = {
  f : int;
  b : int;
  t : int;
  r : int;
  c : int;
  rides : Ride.t array;
}

let mk f b t r c rides = { f; b; t; r; c; rides; }

let finish_time t ride time point =
  let d = Point.dist point ride.Ride.start in
  (max (time + d) ride.Ride.first) + ride.Ride.dist

let score t ride time point =
  let d = Point.dist point ride.Ride.start in
  let bonus = if time + d <= ride.Ride.first then t.b else 0 in
  bonus + ride.Ride.dist

let pp_solution fmt a =
  Array.iter (fun a ->
      let n = Array.length a in
      Format.fprintf fmt "%d " n;
      Array.iter (fun r ->
          Format.fprintf fmt "%d " r.Ride.id
        ) a;
      Format.fprintf fmt "@.") a

let write_solution score sol =
  let out = open_out (Format.sprintf "output/sol.%d" score) in
  let fmt = Format.formatter_of_out_channel out in
  pp_solution fmt sol
