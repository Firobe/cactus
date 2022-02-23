let show_temperature t = Printf.printf "Current temperature: %g°C\n" t

let log_temperature goal temp heating =
  (* timestamp temp heating *)
  let logfile = "/home/pi/Projects/cactus/cactus.log" in
  let chan = open_out_gen [Open_creat; Open_append] 0o644 logfile in
  let timestamp = Unix.time () |> Int64.of_float |> Int64.to_string in
  Printf.fprintf chan "%s %g %d %g\n" timestamp temp
    (if heating then 1 else 0)
    goal ;
  close_out chan

let ( let* ) = Lwt.bind

let margin = 0.5 (* margin below goal temperature (in °C) *)
let active_period = 60. (* length of a heat cycle *)

let should_keep_heating current goal = current < goal
let should_keep_waiting current goal = current >= goal -. margin

let heat_once () =
  Io.select Mode.Active ;
  let* _ = Io.sleep ~blink_mode:Mode.Idle active_period in
  Lwt.return_unit

let rec idle_loop t =
  if Server.get_status () then (
    Printf.printf "Turned on!\n%!" ;
    heat_goal t (Server.get_goal ())
  ) else (
    Io.select Mode.Disabled ;
    let* _ = Io.sleep ~blink_mode:Mode.Disabled 2. in
    idle_loop t
  )

and manage_server t =
  if Server.has_changed () then (
    Printf.printf "Updating goal!\n%!" ;
    heat_goal t (Server.get_goal ())
  ) else if not (Server.get_status ()) then (
    Printf.printf "Turned off!\n%!" ;
    idle_loop t
  ) else Lwt.return_unit

and heat_goal t goal =
  let* _ = manage_server t in
  let current = Temperature.get t in
  if should_keep_heating current goal then (
    show_temperature current ;
    log_temperature goal current true ;
    let* () = heat_once () in
    heat_goal t goal
  )
  else (
    Printf.printf "NOW WAITING\n" ;
    wait_goal t goal
  )

and wait_goal t goal =
  let* _ = manage_server t in
  let current = Temperature.get t in
  if should_keep_waiting current goal then (
    show_temperature current ;
    log_temperature goal current false ;
    Io.select Mode.Idle ;
    let* _ = Io.sleep ~blink_mode:Mode.Disabled 60. in
    wait_goal t goal
  )
  else (
    Printf.printf "NOW HEATING\n" ;
    heat_goal t goal
  )

let test_routine () =
  Io.select Active ;
  let* _ = Io.sleep 2. in
  Io.select Idle ;
  let* _ = Io.sleep 2. in
  Io.select Disabled ;
  let* _ = Io.sleep 2. in
  Io.select Active ;
  let* _ = Io.sleep 0.1 in
  Io.select Idle ;
  let* _ = Io.sleep 0.1 in
  Io.select Active ;
  let* _ = Io.sleep 0.1 in
  Io.select Idle ;
  let* _ = Io.sleep 0.1 in
  Io.select Active ;
  let* _ = Io.sleep 0.1 in
  Io.select Idle ;
  let* _ = Io.sleep 0.1 in
  let temp = Temperature.init () |> Temperature.get in
  if temp < 6. || temp > 30. then Io.select Disabled else Io.reset () ;
  Lwt.return_unit

let usage () =
  Printf.printf "Usage: cactus select [on|off]\n" ;
  Printf.printf "       cactus server [INIT_TEMPERATURE]\n" ;
  Printf.printf "       cactus read\n" ;
  Printf.printf "       cactus test\n"

let launch_daemon driver initial_goal =
  Lwt_main.run
    (Lwt.join [Server.init initial_goal driver; heat_goal driver initial_goal])

let default_server_temperature = 18.

let main =
  Io.init () ;
  let argc = Array.length Sys.argv in
  if argc >= 2 then
    match Sys.argv.(1) with
    | "select" ->
        if argc <> 3 then usage ()
        else Sys.argv.(2) |> Mode.of_string |> Io.select
    | "server" -> (
        if argc = 2 then
          launch_daemon (Temperature.init ()) default_server_temperature
        else if argc = 3 then
          match float_of_string_opt Sys.argv.(2) with
          | None -> failwith "Temperature must be a float"
          | Some goal -> launch_daemon (Temperature.init ()) goal
        else usage ()
      )
    | "read" -> Temperature.init () |> Temperature.get |> show_temperature
    | "test" -> Lwt_main.run (test_routine ())
    | _ -> failwith "Unknown command"
  else usage ()
