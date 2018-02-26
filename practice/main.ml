
let () =
  let state = Input.read_state () in
  let res = Dyn.dyn_dyn state 4 in
  Format.eprintf "score: %d@." (State.score res);
  Format.printf "%a@." State.pp_solution res

