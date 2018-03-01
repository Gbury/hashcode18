
type t = {
  x : int;
  y : int;
}

let dist p p' = abs (p.x - p'.x) + abs (p.y - p'.y)

let mk x y = { x; y; }


