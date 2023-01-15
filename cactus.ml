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
  transitions : (state_id * (float -> Signatures.state -> bool)) list;
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
let default_server_temperature = 18.

module Make
    (Server : Signatures.Server)
    (Temp : Signatures.Temperature)
    (Gpio : Signatures.IO) =
struct
  module S = Server (Temp)

  let rec exec t io state =
    let descr = List.assoc state automaton in
    let* cur = Temp.get t in
    let server_state = S.state () in
    match
      List.find_opt (fun (_, f) -> f cur server_state) descr.transitions
    with
    | Some (next, _) ->
        Printf.printf "Transition to %s\n%!" (show_state_id next);
        exec t io next (* transition to another state *)
    | None ->
        (* show_Temp cur ; *)
        (* log_Temp state.goal cur (state = Heat) ; *)
        Gpio.select io descr.mode;
        let* () = Gpio.sleep io ~blink_mode:descr.blink quantum in
        exec t io state

  let get_io () =
    match Gpio.init () with
    | Ok t -> t
    | Error (`Msg msg) ->
        failwith (Printf.sprintf "Couldn't open GPIO: %s\n" msg)

  let test_routine () =
    let io = get_io () in
    Gpio.select io Active;
    let* _ = Gpio.sleep io 2. in
    Gpio.select io Idle;
    let* _ = Gpio.sleep io 2. in
    Gpio.select io Disabled;
    let* _ = Gpio.sleep io 2. in
    Gpio.select io Active;
    let* _ = Gpio.sleep io 0.1 in
    Gpio.select io Idle;
    let* _ = Gpio.sleep io 0.1 in
    Gpio.select io Active;
    let* _ = Gpio.sleep io 0.1 in
    Gpio.select io Idle;
    let* _ = Gpio.sleep io 0.1 in
    Gpio.select io Active;
    let* _ = Gpio.sleep io 0.1 in
    Gpio.select io Idle;
    let* _ = Gpio.sleep io 0.1 in
    let* temp = Temp.init () |> Temp.get in
    if temp < 6. || temp > 30. then Gpio.select io Disabled else Gpio.reset io;
    Lwt.return_unit

  let launch_daemon initial_goal =
    let driver = Temp.init () in
    let io = get_io () in
    Lwt_main.run
      (Lwt.choose [ S.init initial_goal driver; exec driver io Wait ])
end

let usage () =
  Printf.printf "Usage: cactus server [INIT_Temp]\n";
  Printf.printf "       cactus test\n";
  Printf.printf "Set CACTUS_DUMMY=y to use dummy IO\n";
  Printf.printf "Set CACTUS_TELNET=y to use the old unsecure telnet interface\n"

let main =
  let server =
    match Sys.getenv_opt "CACTUS_TELNET" with
    | Some "y" -> (module Telnet.Make : Signatures.Server)
    | _ -> (module Rest.Make)
  in
  let temp, io =
    match Sys.getenv_opt "CACTUS_DUMMY" with
    | Some "y" ->
        ( (module Dummy.Temperature : Signatures.Temperature),
          (module Dummy.IO : Signatures.IO) )
    | _ -> ((module Temperature), (module Io))
  in
  let module T = (val temp) in
  let module I = (val io) in
  let module S = (val server) in
  let module Main = Make (S) (T) (I) in
  let argc = Array.length Sys.argv in
  if argc >= 2 then
    match Sys.argv.(1) with
    | "server" ->
        if argc = 2 then Main.launch_daemon default_server_temperature
        else if argc = 3 then
          match float_of_string_opt Sys.argv.(2) with
          | None -> failwith "Temperature must be a float"
          | Some goal -> Main.launch_daemon goal
        else usage ()
    | "test" -> Lwt_main.run (Main.test_routine ())
    | _ -> failwith "Unknown command"
  else usage ()
