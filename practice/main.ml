
let () =
  let s = Input.read_state () in
  Format.printf "%a@." State.pp_stat_matrix s.sums

