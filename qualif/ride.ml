
type t = {
  id : int;
  start : Point.t;
  stop : Point.t;
  dist : int;
  first : int;
  last : int;
}

let equal r r' = r.id = r'.id
let compare r r' = compare r.id r'.id
module Set = Set.Make(struct type nonrec t = t let compare = compare end)
module Map = Map.Make(struct type nonrec t = t let compare = compare end)

let pp_id fmt r = Format.fprintf fmt "%d" r.id

let mk id a b x y s f =
  let start = Point.mk a b in
  let stop = Point.mk x y in
  let dist = Point.dist start stop in
  let first = s in
  let last = f - dist in
  { id; start; stop; dist; first; last; }

let is_feasible t time = time <= t.last

let is_reachable t time point =
  let d = Point.dist point t.start in
  is_feasible t (time + d)


