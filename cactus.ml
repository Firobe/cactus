let show_temperature t = Printf.printf "Current temperature: %g°C\n" t

let log_temperature goal temp heating =
  (* timestamp temp heating *)
  let logfile = "/home/pi/Projects/cactus/cactus.log" in
  let chan = open_out_gen [ Open_creat; Open_append ] 0o644 logfile in
  let timestamp = Unix.time () |> Int64.of_float |> Int64.to_string in
  Printf.fprintf chan "%s %g %d %g\n" timestamp temp
    (if heating then 1 else 0)
    goal;
  close_out chan

let ( let* ) = Lwt.bind
let margin = 0.5 (* margin below goal temperature (in °C) *)
let quantum = 2. (* maximum reactivity time *)

(* Cactus state machine *)

type state_id = Wait | Off | Heat [@@deriving show]

type state_descr = {
  mode : Mode.t;
  blink : Mode.t;
  transitions : (state_id * (float -> Server.state -> bool)) list;
}

let off_state =
  {
    mode = Mode.Disabled;
    blink = Mode.Disabled;
    transitions = [ (Heat, fun _ { on; _ } -> on) ];
  }

let heat_state =
  {
    mode = Mode.Active;
    blink = Mode.Idle;
    transitions =
      [
        (Off, fun _ { on; _ } -> not on);
        (Wait, fun cur { goal; _ } -> cur >= goal);
      ];
  }

let wait_state =
  {
    mode = Mode.Idle;
    blink = Mode.Disabled;
    transitions =
      [
        (Off, fun _ { on; _ } -> not on);
        (Heat, fun cur { goal; _ } -> cur < goal -. margin);
      ];
  }

let automaton = [ (Off, off_state); (Heat, heat_state); (Wait, wait_state) ]

let rec exec t state =
  let descr = List.assoc state automaton in
  let* cur = Temperature.get t in
  let server_state = Server.state () in
  match List.find_opt (fun (_, f) -> f cur server_state) descr.transitions with
  | Some (next, _) ->
      Printf.printf "Transition to %s\n%!" (show_state_id next);
      exec t next (* transition to another state *)
  | None ->
      (* show_temperature cur ; *)
      (* log_temperature state.goal cur (state = Heat) ; *)
      Io.select descr.mode;
      let* () = Io.sleep ~blink_mode:descr.blink quantum in
      exec t state

let test_routine () =
  Io.select Active;
  let* _ = Io.sleep 2. in
  Io.select Idle;
  let* _ = Io.sleep 2. in
  Io.select Disabled;
  let* _ = Io.sleep 2. in
  Io.select Active;
  let* _ = Io.sleep 0.1 in
  Io.select Idle;
  let* _ = Io.sleep 0.1 in
  Io.select Active;
  let* _ = Io.sleep 0.1 in
  Io.select Idle;
  let* _ = Io.sleep 0.1 in
  Io.select Active;
  let* _ = Io.sleep 0.1 in
  Io.select Idle;
  let* _ = Io.sleep 0.1 in
  let* temp = Temperature.init () |> Temperature.get in
  if temp < 6. || temp > 30. then Io.select Disabled else Io.reset ();
  Lwt.return_unit

let usage () =
  Printf.printf "Usage: cactus select [on|off]\n";
  Printf.printf "       cactus server [INIT_TEMPERATURE]\n";
  Printf.printf "       cactus read\n";
  Printf.printf "       cactus test\n"

let launch_daemon driver initial_goal =
  Lwt_main.run (Lwt.join [ Server.init initial_goal driver; exec driver Wait ])

let default_server_temperature = 18.

let main =
  Io.init ();
  let argc = Array.length Sys.argv in
  if argc >= 2 then
    match Sys.argv.(1) with
    | "select" ->
        if argc <> 3 then usage ()
        else Sys.argv.(2) |> Mode.of_string |> Io.select
    | "server" ->
        if argc = 2 then
          launch_daemon (Temperature.init ()) default_server_temperature
        else if argc = 3 then
          match float_of_string_opt Sys.argv.(2) with
          | None -> failwith "Temperature must be a float"
          | Some goal -> launch_daemon (Temperature.init ()) goal
        else usage ()
    | "test" -> Lwt_main.run (test_routine ())
    | _ -> failwith "Unknown command"
  else usage ()
