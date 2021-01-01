let period = 2. *. 60. (* seconds *)
let active_duty = 0.20 (* [0 - 1] *)

let rec active_loop () =
  let active_length = active_duty *. period in
  let standby_length = (1. -. active_duty) *. period in
  Io.select Mode.Active;
  Io.sleep ~blink_mode:Mode.Idle active_length;
  Io.select Mode.Idle;
  Io.sleep ~blink_mode:Mode.Disabled standby_length;
  active_loop ()

let main =
  Io.init ();
  let t = Temperature.init () in
  Printf.printf "Current temperature: %f\n" (Temperature.get t);
  if ((Array.length Sys.argv) <> 2) then (
    Printf.printf "Auto mode enabled\n%!";
    active_loop ()
  ) else
    Sys.argv.(1) |> Mode.of_string |> Io.select
