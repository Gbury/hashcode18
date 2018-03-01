


let () =
  let state = Input.read_input Scanf.Scanning.stdin in
  let res = Far.far_block state 50 in
  Format.printf "%a" Far.pp res


