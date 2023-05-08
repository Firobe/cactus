open Mirage

let stack = generic_stackv4v6 default_network
let conduit = conduit_direct ~tls:true stack
let server = cohttp_server conduit
let resolver = resolver_dns stack
let client = cohttp_client resolver conduit

let port =
  let doc = Key.Arg.info ~doc:"Port for the HTTP server" [ "port" ] in
  Key.(create "port" Arg.(opt int 80 doc))

let host =
  let doc = Key.Arg.info ~doc:"Host of the cactus heater" [ "host" ] in
  Key.(create "host" Arg.(opt string "home.firobe.fr" doc))

let host_port =
  let doc = Key.Arg.info ~doc:"Host of the cactus heater" [ "host-port" ] in
  Key.(create "host_port" Arg.(opt int 2713 doc))

let password =
  let doc = Key.Arg.info ~doc:"Host of the cactus heater" [ "password" ] in
  Key.(create "password" Arg.(required string doc))

let assets = crunch "assets"

let main =
  main
    ~keys:[ key port; key host; key host_port; key password ]
    ~packages:[ package "tyxml" ]
    "Unikernel.Make"
    (http @-> http_client @-> kv_ro @-> job)

let () = register "cactus_web" [ main $ server $ client $ assets ]
