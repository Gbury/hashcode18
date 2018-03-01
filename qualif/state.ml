
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


