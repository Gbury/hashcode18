
let () =
  if Array.length Sys.argv > 2 then begin
    (* Analyze mode *)
    let state = Input.read_state (Scanf.Scanning.open_in Sys.argv.(1)) in
    let sol = Input.read_solution (Scanf.Scanning.open_in Sys.argv.(2)) in
    let unused = Solutionreader.notsliced sol state.State.pizza in
    Format.printf "@[<hv>unused points:@ %a@]"
      CCFormat.(list ~sep:(return "@ ") (pair int int)) unused
  end else begin
    let state = Input.read_state Scanf.Scanning.stdin in
    let res = Dyn.dyn_dyn state 4 in
    Format.eprintf "score: %d@." (State.score res);
    Format.printf "%a@." State.pp_solution res
  end

