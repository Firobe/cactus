type t = { mutable last_temp : float }

let adjust _ _ = Lwt.return 0.
let offset _ = 0.
let addr = "::"
let port = 5683

let respond ~to_ ~status =
  Coap.Message.(
    make ~id:(id to_) ~kind:Acknowledgement ~code:(Response status)
      (buffer_of_string "uwu"))

let temp_of_int u = Float.of_int u /. 10.

let handle t m =
  let open Coap in
  match Message.code m with
  | Request (`Put | `Post) ->
      let payload = Cstruct.of_bigarray (Message.payload m) in
      let temp = Cstruct.BE.get_uint16 payload 0 |> temp_of_int in
      Format.printf "[coap] got temp data: %f°C@." temp;
      t.last_temp <- temp;
      let resp = respond ~to_:m ~status:`Created in
      Lwt.return resp
  | _ -> Lwt.return (respond ~to_:m ~status:`Not_implemented)

let server t () =
  Coap.Server.start ~addr ~port (fun r -> Result.get_ok r |> handle t)

let init () =
  let t = { last_temp = 69. } in
  Lwt.async (server t);
  t

let get t = Lwt.return t.last_temp
