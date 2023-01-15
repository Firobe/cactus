open Cohttp_lwt_unix
open Signatures

module Make (Temp : Signatures.Temperature) = struct
  type endpoint = Root | Goal | Power | Temp

  let routes =
    Routes.
      [
        nil @--> Root;
        (s "goal" /? nil) @--> Goal;
        (s "power" /? nil) @--> Power;
        (s "temp" /? nil) @--> Temp;
      ]

  let validate_temperature v =
    if v >= 28. then Result.error (`Bad_request, "Too high")
    else if v < 0. then Result.error (`Bad_request, "Must be positive")
    else Result.ok v

  let goal = ref 20.
  let power = ref true

  let root () =
    Format.asprintf "Welcome, I'm a heater! Available endpoints are:@.%a@."
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline Routes.pp_route)
      routes

  let get_val conv name body =
    try conv body |> Result.ok
    with Failure _ ->
      let msg =
        Printf.sprintf "Expected a body of type %s but got '%s'" name body
      in
      Result.error (`Bad_request, msg)

  let get_float = get_val float_of_string "float"
  let get_bool = get_val bool_of_string "bool"
  let ( let* ) = Lwt.bind
  let ( let+ ) = Result.bind
  let r = Lwt.return
  let rmerge = function Ok v -> r v | Error v -> r v

  let serve t endpoint meth body =
    match (endpoint, meth) with
    | Root, `GET -> r (`OK, root ())
    | Goal, `GET -> r (`OK, string_of_float !goal)
    | Goal, `POST ->
        (let+ v = get_float body in
         let+ v = validate_temperature v in
         goal := v;
         Result.ok (`No_content, ""))
        |> rmerge
    | Power, `GET -> r (`OK, string_of_bool !power)
    | Power, `POST ->
        (let+ v = get_bool body in
         power := v;
         Result.ok (`No_content, ""))
        |> rmerge
    | Temp, `GET ->
        let* temp = Temp.get t in
        r (`OK, string_of_float temp)
    | Temp, `POST -> (
        match get_float body with
        | Ok v ->
            let* _ = Temp.adjust v t in
            r (`No_content, "")
        | Error e -> r e)
    | _ -> r (`Method_not_allowed, "Method not allowed on this endpoint")

  let state () = { on = !power; goal = !goal }
  let port = 8000

  let init g temp =
    goal := g;
    let callback _conn req body =
      let target = Request.uri req |> Uri.path in
      let meth = Request.meth req in
      let* body = Cohttp_lwt.Body.to_string body in
      let* status, body =
        match Routes.(match' (one_of routes) ~target) with
        | FullMatch s | MatchWithTrailingSlash s -> serve temp s meth body
        | NoMatch ->
            Lwt.return
              (`Bad_request, Printf.sprintf "No matching route for %s" target)
      in
      Server.respond_string ~status ~body ()
    in
    let tls_config =
      ( `Crt_file_path "self.crt",
        `Key_file_path "self.key",
        `No_password,
        `Port port )
    in
    Printf.printf "Listening (REST) on port %d...\n%!" port;
    Server.create ~mode:(`TLS tls_config) (Server.make ~callback ())
end
