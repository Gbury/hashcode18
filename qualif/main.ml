


let () =
  let state = Input.read_input Scanf.Scanning.stdin in
  let res = Far.far_block state 10 in
  Format.printf "%a" State.pp_solution res


