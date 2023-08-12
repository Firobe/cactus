type signal = Low | High
type chip = Gpiod.gpiod_chip Ctypes.structure Ctypes_static.ptr
type line = Gpiod.gpiod_line Ctypes.structure Ctypes_static.ptr
type t = { chip : chip; active : line; idle : line; disabled : line }

let ( let+ ) = Result.bind
let t none = Option.to_result ~none:(`Msg none)
let nz msg v = if v = 0 then Result.ok () else Result.error (`Msg msg)

let gpiod_write t mode v =
  let open Gpiod in
  let line =
    match mode with
    | Mode.Active -> t.active
    | Idle -> t.idle
    | Disabled -> t.disabled
  in
  let+ () = line_set_value (Some line) v |> nz "couldn't set value" in
  Result.ok ()

let write t m x =
  let v = match x with High -> 1 | Low -> 0 in
  match gpiod_write t m v with
  | Ok () -> ()
  | Error (`Msg msg) -> Printf.printf "Couldn't set line: %s!\n%!" msg

let reset t =
  write t Mode.Active Low;
  write t Mode.Disabled Low;
  write t Mode.Idle Low

let select t m =
  (* Printf.printf "Switching to mode %s\n%!" (Mode.show m) ; *)
  reset t;
  write t m High

let get_init_state () =
  let open Gpiod in
  let+ chip = chip_open_by_name (Some "gpiochip0") |> t "couldn't open chip" in
  let get_and_request line =
    let+ line = chip_get_line (Some chip) line |> t "couldn't get line" in
    let+ () =
      line_request_output (Some line) (Some "example") 0
      |> nz "couldn't request line"
    in
    Result.ok line
  in
  let+ active = get_and_request Mode.(get_pin Active) in
  let+ disabled = get_and_request Mode.(get_pin Disabled) in
  let+ idle = get_and_request Mode.(get_pin Idle) in
  Result.ok { chip; active; idle; disabled }

let init () =
  let+ t = get_init_state () in
  select t Mode.Disabled;
  Result.ok t

let ( let* ) = Lwt.bind

let sleep t ?blink_mode ?(blink_interval = 0.9) ?(blink_duration = 0.1) s =
  match blink_mode with
  | None -> Lwt_unix.sleep s
  | Some mode ->
      let sum = blink_interval +. blink_duration in
      let rec aux left =
        if left < sum then Lwt_unix.sleep left
        else (
          write t mode High;
          let* _ = Lwt_unix.sleep blink_duration in
          write t mode Low;
          let* _ = Lwt_unix.sleep blink_interval in
          aux (left -. sum))
      in
      aux s
