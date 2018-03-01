
let read_int ic =
  Scanf.bscanf ic " %d " (fun x -> x)

let read_ride ic i gf =
  let a = read_int ic in
  let b = read_int ic in
  let x = read_int ic in
  let y = read_int ic in
  let s = read_int ic in
  let f = read_int ic in
  Ride.mk i a b x y s (min f gf)

let read_input ic =
  let r = read_int ic in
  let c = read_int ic in
  let f = read_int ic in
  let n = read_int ic in
  let b = read_int ic in
  let t = read_int ic in
  let a = Array.init n (fun i -> read_ride ic i t) in
  State.mk f b t r c a

let read_solution state ic =
  Array.init n (fun _ ->
      let m = read_int ic in
      Array.init m (fun _ ->
          let i = read_int ic in
          State.(state.rides.(i))
        )
    )
