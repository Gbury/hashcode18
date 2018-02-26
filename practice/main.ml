
let () =
  let s = Input.read_state () in
  Format.printf "%a@." State.pp_int_matrix s.sums

