open Lwt.Syntax

module type SERVER = Cohttp_mirage.Server.S
module type CLIENT = Cohttp_lwt.S.Client

type state = { power : bool; goal : float; temp : float }

module State (Client : CLIENT) = struct
  let headers () =
    let password = Key_gen.password () in
    let credential = `Basic ("virgile", password) in
    Cohttp.Header.(add_authorization (init ()) credential)

  let ( let** ) = Lwt_result.bind

  let endpoint path =
    let host = Key_gen.host () in
    let port = Key_gen.host_port () in
    let scheme = "https" in
    Uri.make ~scheme ~host ~port ~path ()

  let check_error (resp, body) endpoint =
    if Cohttp.(Code.is_error (Response.status resp |> Code.code_of_status)) then
      let* err = Cohttp_lwt.Body.to_string body in
      let msg = Printf.sprintf "request to %s failed: %s" endpoint err in
      Lwt_result.fail (`Msg msg)
    else Lwt_result.return ()

  let get_endpoint ~client ~parse x =
    let uri = endpoint x in
    Logs.info (fun f -> f "GET %s" (Uri.to_string uri));
    let headers = headers () in
    let* ((_, body) as r) = Client.get ~ctx:client ~headers uri in
    let** () = check_error r x in
    let* body_s = Cohttp_lwt.Body.to_string body in
    Lwt.return (parse body_s)

  let parse_bool x =
    bool_of_string_opt x |> Option.to_result ~none:(`Msg "couldn't parse bool")

  let parse_float x =
    Float.of_string_opt x
    |> Option.to_result ~none:(`Msg ("couldn't parse float: " ^ x))

  let power client = get_endpoint ~client ~parse:parse_bool "/power"
  let temp client = get_endpoint ~client ~parse:parse_float "/temp"
  let goal client = get_endpoint ~client ~parse:parse_float "/goal"

  let get client =
    Lwt.catch
      (fun () ->
        let** power = power client in
        let** goal = goal client in
        let** temp = temp client in
        Lwt_result.return { power; goal; temp })
      (fun exn ->
        let msg = Printexc.to_string exn in
        let msg = "Exception when getting state: " ^ msg in
        Lwt_result.fail (`Msg msg))

  let post client x body =
    let body = Cohttp_lwt.Body.of_string body in
    let uri = endpoint x in
    let headers = headers () in
    let* r = Client.post ~headers ~ctx:client ~body uri in
    check_error r x

  let turn client v = post client "/power" (string_of_bool v)
  let change client v = post client "/goal" (string_of_float v)
end

module Page (Assets : Mirage_kv.RO) = struct
  open Tyxml
  open Html

  let of_temp f =
    div ~a:[ a_class [ "data" ] ] [ txt (Printf.sprintf "%.1f degrees" f) ]

  let of_power b =
    div ~a:[ a_class [ "data" ] ] [ txt (if b then "on" else "off") ]

  let status st =
    div
      ~a:[ a_class [ "block" ] ]
      [
        h2 [ txt "Current status" ];
        ul
          [
            li [ b [ txt "Power: " ]; of_power st.power ];
            li [ b [ txt "Temperature: " ]; of_temp st.temp ];
            li [ b [ txt "Goal: " ]; of_temp st.goal ];
          ];
      ]

  let change_form_name = "new-temp"

  let change_temp st =
    form
      ~a:[ a_method `Post; a_action "change" ]
      [
        button [ txt "Change goal" ];
        input
          ~a:
            [
              a_name change_form_name;
              a_input_type `Number;
              a_step (Some 0.1);
              a_value (Float.to_string st.goal);
            ]
          ();
      ]

  let switch st =
    let label = if st.power then "off" else "on" in
    form
      ~a:[ a_method `Post; a_action label ]
      [ button [ txt ("Turn " ^ label) ] ]

  let controls st =
    div
      ~a:[ a_class [ "block" ] ]
      [ h2 [ txt "Controls" ]; change_temp st; switch st ]

  let contents = function
    | `Home st -> [ status st; controls st ]
    | `Error msg -> [ p [ b [ txt "SERVER ERROR: " ]; txt msg ] ]

  let page assets st =
    let* css = Assets.get assets (Mirage_kv.Key.v "style.css") in
    let css = Result.get_ok css in
    Lwt.return
    @@ html
         (head
            (title (txt "Cactus controller"))
            [
              meta ~a:[ a_charset "UTF-8" ] ();
              meta
                ~a:
                  [
                    a_name "viewport";
                    a_content "width=device-width,initial-scale=1.0";
                  ]
                ();
              style [ txt css ];
            ])
         (body (contents st))

  let render assets st =
    let* page = page assets st in
    Lwt.return (Format.asprintf "%a" (pp ()) page)
end

module Dispatch (Server : SERVER) (Client : CLIENT) (Assets : Mirage_kv.RO) =
struct
  module S = State (Client)
  module P = Page (Assets)

  let callback client assets _conn req body =
    let ( let** ) = Lwt_result.bind in
    let password = Key_gen.password () in
    let headers = Cohttp.Request.headers req in
    match Cohttp.Header.get_authorization headers with
    | Some (`Basic ("virgile", pass)) when pass = password -> (
        let uri = Cohttp.Request.uri req |> Uri.canonicalize in
        let respond_render state =
          let* body = P.render assets state in
          Server.respond ~status:`OK ~body:(`String body) ()
        in
        let redirect_home () =
          let base =
            match Cohttp.Header.get headers "X-Forwarded-Host" with
            | Some base -> Uri.of_string base
            | None -> Uri.with_path uri ""
          in
          Server.respond_redirect ~uri:base ()
        in
        let* res =
          match Uri.path uri with
          | "/" ->
              let** state = S.get client in
              respond_render (`Home state) |> Lwt_result.ok
          | "/on" ->
              let** () = S.turn client true in
              redirect_home () |> Lwt_result.ok
          | "/off" ->
              let** () = S.turn client false in
              redirect_home () |> Lwt_result.ok
          | "/change" ->
              let* form = Cohttp_lwt.Body.to_form body in
              let** temp =
                List.find_map
                  (fun (k, v) ->
                    if k = P.change_form_name then Some (List.hd v) else None)
                  form
                |> Option.to_result ~none:(`Msg "form element not found")
                |> Lwt.return
              in
              let** temp = S.parse_float temp |> Lwt.return in
              let** () = S.change client temp in
              redirect_home () |> Lwt_result.ok
          | _ -> respond_render (`Error "Page not found") |> Lwt_result.ok
        in
        match res with
        | Ok x -> Lwt.return x
        | Error (`Msg msg) -> respond_render (`Error msg))
    | _ -> Server.respond_need_auth ~auth:(`Basic "cactus") ()

  let go client assets = Server.make ~callback:(callback client assets) ()
end

module Make (Server : SERVER) (Client : CLIENT) (Assets : Mirage_kv.RO) = struct
  module D = Dispatch (Server) (Client) (Assets)

  let start server client assets =
    let port = Key_gen.port () in
    server (`TCP port) (D.go client assets)
end
