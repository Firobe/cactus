open Lwt
open Signatures

module Make (Temp : Signatures.Temperature) = struct
  let current_goal = ref 0.
  let currently_enabled = ref true

  let validate_temperature v =
    match float_of_string_opt v with
    | Some goal when goal >= 28. -> Result.error (`Msg "Too high")
    | Some goal when goal < 0. -> Result.error (`Msg "Must be positive")
    | Some goal -> Result.ok goal
    | None -> Result.error (`Msg "Float expected")

  let ( let* ) = Lwt.bind

  let handle_message temp msg =
    match String.split_on_char ' ' msg with
    | [ "get" ] -> `Reply (string_of_float !current_goal) |> Lwt.return
    | [ "set"; v ] -> (
        match validate_temperature v with
        | Result.Ok goal ->
            if !current_goal <> goal then (
              Printf.printf "Received a new goal: %g\n%!" goal;
              current_goal := goal;
              `Reply "Goal changed" |> Lwt.return)
            else `Reply "Same goal" |> Lwt.return
        | Result.Error (`Msg m) -> `Reply m |> Lwt.return)
    | [ "turn"; "on" ] ->
        if !currently_enabled then `Reply "Already on." |> Lwt.return
        else (
          currently_enabled := true;
          `Reply "Turned on." |> Lwt.return)
    | [ "turn"; "off" ] ->
        if not !currently_enabled then `Reply "Already off." |> Lwt.return
        else (
          currently_enabled := false;
          `Reply "Turned off." |> Lwt.return)
    | [ "adjust"; v ] -> (
        match validate_temperature v with
        | Result.Ok cal ->
            let* off = Temp.adjust cal temp in
            let s = Printf.sprintf "Registered offset of %g°C" off in
            Lwt.return @@ `Reply s
        | Result.Error (`Msg m) -> `Reply m |> Lwt.return)
    | [ "read" ] ->
        let* v = Temp.get temp in
        let s = !currently_enabled in
        let offset = Temp.offset temp in
        let m =
          Printf.sprintf "Temp: %g°C (offset %g) Enabled: %B" v offset s
        in
        `Reply m |> Lwt.return
    | [ "exit" ] -> Lwt.return `Close
    | [ "help" ] ->
        Lwt.return
        @@ `Reply
             "help           Get this message\n\
              exit           End Telnet session\n\
              get            Get current goal\n\
              turn on/off    Enable enable/disable\n\
              set TEMP       Set current goal\n\
              adjust TEMP    Calibrate with real room temperature\n\
              read           Read actual room temperature"
    | _ -> `Reply "Invalid command" |> Lwt.return

  let listen_address = Unix.inet_addr_any
  let port = 2713

  let create_socket () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock SO_REUSEADDR true;
    let* () = bind sock @@ ADDR_INET (listen_address, port) in
    listen sock 3;
    return sock

  let handle_connection temp fd ic oc =
    let rec repl () =
      let* () = Lwt_io.write oc "> " in
      let* msg = Lwt_io.read_line_opt ic in
      match msg with
      | Some msg -> (
          let* reaction = handle_message temp msg in
          match reaction with
          | `Reply reply ->
              let* () = Lwt_io.write_line oc reply in
              repl ()
          | `Close -> Lwt_unix.close fd)
      | None ->
          Printf.printf "Connection closed\n%!";
          return_unit
    in
    (* let* () = Lwt_io.write_line oc motd in *)
    repl ()

  let accept_connection temp conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    Lwt.on_failure (handle_connection temp fd ic oc) (fun e ->
        Printf.printf "Connection error: %s\n" (Printexc.to_string e));
    Printf.printf "New connection\n%!";
    return_unit

  let state () = { on = !currently_enabled; goal = !current_goal }

  let init _ goal temp =
    current_goal := goal;
    let* sock = create_socket () in
    let rec serve () =
      Printf.printf "Listening (TELNET) on port %d...\n%!" port;
      let* conn = Lwt_unix.accept sock in
      let* _ = accept_connection temp conn in
      serve ()
    in
    serve ()
end
