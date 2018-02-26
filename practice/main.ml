
let () =
  let state = Input.read_state () in
  let res = Dyn.solve state 3 in
  Format.eprintf "score: %d@." (State.score res);
  Format.printf "%a@." State.pp_solution res

