


let () =
  let state = Input.read_input Scanf.Scanning.stdin in
  let res = Population.solve state in
  Format.printf "%a@." State.pp_solution res;
  Format.eprintf "Ok@."


