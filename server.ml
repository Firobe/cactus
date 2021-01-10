open Lwt

let current_goal = ref 0.
let goal_has_changed = ref false

let handle_message msg =
  match float_of_string_opt msg with
  | Some goal ->
      if !current_goal <> goal then (
        goal_has_changed := true ;
        Printf.printf "Received a new goal: %g\n%!" goal ;
        current_goal := goal ;
        "Goal changed" )
      else "Same goal"
  | None -> "Float expected"

let listen_address = Unix.inet_addr_any
let port = 2713
let ( let* ) = Lwt.bind

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let* () = bind sock @@ ADDR_INET (listen_address, port) in
  listen sock 3 ; return sock

let rec handle_connection ic oc =
  let* msg = Lwt_io.read_line_opt ic in
  match msg with
  | Some msg ->
      let reply = handle_message msg in
      let* () = Lwt_io.write_line oc reply in
      handle_connection ic oc
  | None ->
      Printf.printf "Connection closed\n%!" ;
      return_unit

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc) (fun e ->
      Printf.printf "Connection error: %s\n" (Printexc.to_string e) ) ;
  Printf.printf "New connection\n%!" ;
  return_unit

let has_changed () =
  let old = !goal_has_changed in
  goal_has_changed := false ;
  old

let get_goal () = !current_goal

let init goal =
  current_goal := goal ;
  let* sock = create_socket () in
  let rec serve () =
    Printf.printf "Listening...\n%!" ;
    let* conn = Lwt_unix.accept sock in
    let* _ = accept_connection conn in
    serve () in
  serve ()
