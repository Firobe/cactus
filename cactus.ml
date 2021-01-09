type pwm_config = {
  period : float; (* seconds *)
  duty : float; (* [0 - 1] *)
}

let active_pwm = {period = 60.; duty = 0.8}

let show_temperature t =
  Printf.printf "Current temperature: %g°C\n" t

let log_temperature goal temp heating =
  (* timestamp temp heating *)
  let logfile = "/home/pi/Projects/cactus/cactus.log" in
  let chan = open_out_gen [Open_creat; Open_append] 0o644 logfile in
  let timestamp = Unix.time () |> Int64.of_float |> Int64.to_string in
  Printf.fprintf chan "%s %g %d %g\n" timestamp temp
    (if heating then 1 else 0) goal;
  close_out chan

let rec active_loop pwm times =
  if times > 0 then begin
    let active_length = pwm.duty *. pwm.period in
    let standby_length = (1. -. pwm.duty) *. pwm.period in
    Io.select Mode.Active;
    let%lwt _ = Io.sleep ~blink_mode:Mode.Idle active_length in
    Io.select Mode.Idle;
    let%lwt _ = Io.sleep ~blink_mode:Mode.Idle standby_length in
    active_loop pwm (times - 1)
  end else Lwt.return_unit

let margin = 0.5 (* margin below goal temperature *)

let rec heat_goal t goal = 
  if Server.has_changed () then begin
    Printf.printf "Updating goal!\n%!";
    heat_goal t (Server.get_goal ())
  end else
    let current = Temperature.get t in
    if current >= goal then begin
      Printf.printf "NOW WAITING\n";
      wait_goal t goal
    end else begin
      show_temperature current;
      log_temperature goal current true;
      let%lwt _ = active_loop active_pwm 1 in
      heat_goal t goal
    end

and wait_goal t goal =
  if Server.has_changed () then begin
    Printf.printf "Updating goal!\n%!";
    heat_goal t (Server.get_goal ())
  end else
    let current = Temperature.get t in
    if current <= (goal -. margin) then begin
      Printf.printf "NOW HEATING\n";
      heat_goal t goal
    end else begin
      show_temperature current;
      log_temperature goal current false;
      Io.select Mode.Idle;
      let%lwt _ = Io.sleep ~blink_mode:Mode.Disabled 60. in
      wait_goal t goal
    end

let test_routine () =
  Io.select Active;
  let%lwt _ = Io.sleep 2. in
  Io.select Idle;
  let%lwt _ = Io.sleep 2. in
  Io.select Disabled;
  let%lwt _ = Io.sleep 2. in
  Io.select Active;
  let%lwt _ = Io.sleep 0.1 in
  Io.select Idle;
  let%lwt _ = Io.sleep 0.1 in
  Io.select Active;
  let%lwt _ = Io.sleep 0.1 in
  Io.select Idle;
  let%lwt _ = Io.sleep 0.1 in
  Io.select Active;
  let%lwt _ = Io.sleep 0.1 in
  Io.select Idle;
  let%lwt _ = Io.sleep 0.1 in
  let temp = Temperature.init () |> Temperature.get in
  if temp < 6. || temp > 30. then
    Io.select Disabled
  else Io.reset ();
  Lwt.return_unit

let usage () =
  Printf.printf "Usage: cactus select [on|off]\n";
  Printf.printf "       cactus goal TEMPERATURE\n";
  Printf.printf "       cactus read\n";
  Printf.printf "       cactus test\n"

let launch_daemon driver initial_goal =
  Lwt_main.run begin
    Lwt.join [Server.init initial_goal; heat_goal driver initial_goal]
  end

let main =
  Io.init ();
  let argc = Array.length Sys.argv in
  if argc >= 2 then
    match Sys.argv.(1) with
    | "select" ->
      if argc <> 3 then usage () else
        Sys.argv.(2) |> Mode.of_string |> Io.select
    | "goal" ->
      if argc <> 3 then usage () else
        begin match float_of_string_opt Sys.argv.(2) with
          | None -> failwith "Temperature must be a float"
          | Some goal -> launch_daemon (Temperature.init ()) goal
        end
    | "read" ->
      Temperature.init () |> Temperature.get |> show_temperature
    | "test" ->
      Lwt_main.run (test_routine ())
    | _ -> failwith "Unknown command"
  else usage()
