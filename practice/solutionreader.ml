
let inslice m s =
  for i = s.State.tl.State.r to s.State.br.State.r do
    for j = s.State.tl.State.c to s.State.br.State.c do
      m.(p.State.r).(p.State.c) <- 1
    done;
  done

let notsliced l pizza =
  let m = Array.make_matrix (Array.length pizza) (Array.length pizza.(0)) 0 in
  List.iter (inslice m) l;
  let res = ref [] in
  for i = 0 to Array.length pizza - 1 do
    for j = 0 to Array.length pizza.(0) - 1 do
      if m.(i).(j) = 0 then res := (i, j) :: !res
    done;
  done;
  !res


