open Lwt

let current_goal = ref 0.
let goal_has_changed = ref false

let validate_temperature v =
  match float_of_string_opt v with
  | Some goal when goal >= 28. -> Result.error (`Msg "Too high")
  | Some goal when goal < 0. -> Result.error (`Msg "Must be positive")
  | Some goal -> Result.ok goal
  | None -> Result.error (`Msg "Float expected")

let motd =
  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n\
   > Cactus heater server | Please don't be evil ! >\n\
   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"

let handle_message temp msg =
  match String.split_on_char ' ' msg with
  | ["get"] -> `Reply (string_of_float !current_goal)
  | ["set"; v] -> begin
      match validate_temperature v with
      | Result.Ok goal ->
        if !current_goal <> goal then (
          goal_has_changed := true ;
          Printf.printf "Received a new goal: %g\n%!" goal ;
          current_goal := goal ;
          `Reply "Goal changed" )
        else `Reply "Same goal"
      | Result.Error (`Msg m) -> `Reply m
    end
  | ["adjust"; v] -> begin
      match validate_temperature v with
      | Result.Ok cal ->
        let off = Temperature.adjust cal temp in
        let s = Printf.sprintf "Registered offset of %g°C" off in
        `Reply s
      | Result.Error (`Msg m) -> `Reply m
    end
  | ["read"] ->
    let v = Temperature.get temp in
    let m = Printf.sprintf "Temp: %g°C (offset %g)" v temp.offset in
    `Reply m
  | ["exit"] -> `Close
  | ["help"] -> `Reply
    "help           Get this message\n\
     exit           End Telnet session\n\
     get            Get current goal\n\
     set TEMP       Set current goal\n\
     adjust TEMP    Calibrate with real room temperature\n\
     read           Read actual room temperature"
  | _ -> `Reply "Invalid command"

let listen_address = Unix.inet_addr_any
let port = 2713
let ( let* ) = Lwt.bind

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock SO_REUSEADDR true ;
  let* () = bind sock @@ ADDR_INET (listen_address, port) in
  listen sock 3 ; return sock

let handle_connection temp fd ic oc =
  let rec repl () =
    let* () = Lwt_io.write oc "> " in
    let* msg = Lwt_io.read_line_opt ic in
    match msg with
    | Some msg ->
      begin match handle_message temp msg with
        | `Reply reply ->
          let* () = Lwt_io.write_line oc reply in
          repl ()
        | `Close -> Lwt_unix.close fd
      end
    | None ->
      Printf.printf "Connection closed\n%!" ;
      return_unit
  in
  let* () = Lwt_io.write_line oc motd in
  repl ()

let accept_connection temp conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_connection temp fd ic oc) (fun e ->
      Printf.printf "Connection error: %s\n" (Printexc.to_string e) ) ;
  Printf.printf "New connection\n%!" ;
  return_unit

let has_changed () =
  let old = !goal_has_changed in
  goal_has_changed := false ;
  old

let get_goal () = !current_goal

let init goal temp =
  current_goal := goal ;
  let* sock = create_socket () in
  let rec serve () =
    Printf.printf "Listening...\n%!" ;
    let* conn = Lwt_unix.accept sock in
    let* _ = accept_connection temp conn in
    serve () in
  serve ()
