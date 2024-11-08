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
    Lwt_main.run
    @@
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

  let error_loop io =
    Gpio.select io Disabled;
    let rec loop () =
      let* _ =
        Gpio.sleep io ~blink_interval:0.3 ~blink_duration:0.3
          ~blink_mode:Disabled 100.
      in
      loop ()
    in
    Lwt_main.run (loop ())

  let launch_daemon initial_goal certs =
    let io = get_io () in
    try
      let driver = Temp.init () in
      Lwt_main.run
        (Lwt.choose [ S.init certs initial_goal driver; exec driver io Wait ])
    with e ->
      Printf.eprintf "Exception raised during loop!\n%s%!"
        (Printexc.to_string e);
      error_loop io
end

open Cmdliner

let usage () =
  Printf.printf "Usage: cactus server [INIT_Temp]\n";
  Printf.printf "       cactus test\n";
  Printf.printf "Set CACTUS_DUMMY=y to use dummy IO\n";
  Printf.printf "Set CACTUS_TELNET=y to use the old unsecure telnet interface\n"

let temperature =
  let doc = "Initial temperature goal" in
  Arg.(
    value & pos 0 float default_server_temperature & info [] ~docv:"TEMP" ~doc)

let telnet =
  let doc = "Use old insecure telnet interface" in
  let env = Cmd.Env.info "CACTUS_TELNET" in
  Arg.(value & flag & info [ "telnet" ] ~doc ~env)

let dummy_io =
  let doc = "Use dummy I/O for GPIO and I2C (for testing)" in
  let env = Cmd.Env.info "CACTUS_DUMMY" in
  Arg.(value & flag & info [ "dummy" ] ~doc ~env)

let coap_sensor =
  let doc = "Use CoAP server to get remote temperature instead of local sensor." in
  let env = Cmd.Env.info "CACTUS_REMOTE_SENSOR" in
  Arg.(value & flag & info [ "remote-sensor" ] ~doc ~env)

let cert =
  let doc = "TLS certificate for the REST server" in
  Arg.(value & opt (some file) None & info [ "c"; "cert" ] ~doc)

let a = Arg.some'

let privkey =
  let doc = "TLS private key for the REST server" in
  Arg.(value & opt (some file) None & info [ "k"; "key" ] ~doc)

module type S = sig
  val launch_daemon : float -> (string * string) option -> unit
  val test_routine : unit -> unit
end

let make_main ~telnet ~dummy_io ~coap_sensor =
  let server =
    if telnet then (module Telnet.Make : Signatures.Server)
    else (module Rest.Make)
  in
  let temp, io =
    if dummy_io then
      ( (if coap_sensor then (module Coap_temp : Signatures.Temperature) else
          (module Dummy.Temperature )),
        (module Dummy.IO : Signatures.IO) )
    else ((if coap_sensor then (module Coap_temp) else (module Temperature)), (module Io))
  in
  let module T = (val temp) in
  let module I = (val io) in
  let module S = (val server) in
  let module Main = Make (S) (T) (I) in
  (module Main : S)

let server_go temp telnet dummy_io coap_sensor cert key =
  let module Main = (val make_main ~telnet ~dummy_io ~coap_sensor) in
  let certs =
    match (telnet, cert, key) with
    | false, Some cert, Some key -> Some (cert, key)
    | false, None, None ->
        Printf.printf "warn: launching REST without https\n";
        None
    | false, _, _ ->
        failwith
          "When using the REST server, -k and -c must be either both passed or \
           not at all\n"
    | true, None, None -> None
    | true, _, _ ->
        Printf.printf "warn: -k and -c are ignored when using telnet\n";
        None
  in
  Main.launch_daemon temp certs

let test_go telnet dummy_io coap_sensor =
  let module Main = (val make_main ~telnet ~dummy_io ~coap_sensor) in
  Main.test_routine ()

let server_cmd =
  let t =
    Term.(const server_go $ temperature $ telnet $ dummy_io $ coap_sensor $ cert $ privkey)
  in
  let doc = "launch daemon" in
  let info = Cmd.info "server" ~doc in
  Cmd.v info t

let test_cmd =
  let t = Term.(const test_go $ telnet $ dummy_io $ coap_sensor) in
  let doc = "test routine (make sure heater is off)" in
  let info = Cmd.info "test" ~doc in
  Cmd.v info t

let cmd =
  let doc = "smart heater server for my home" in
  let info = Cmd.info "cactus" ~doc in
  Cmd.group info [ server_cmd; test_cmd ]

let main () = exit (Cmd.eval cmd)
let () = main ()
