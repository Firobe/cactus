type signal = Low | High

let gpiod_write line v =
  let open Gpiod in
  let t none = Option.to_result ~none:(`Msg none) in
  let nz msg v = if v = 0 then Result.ok () else Result.error (`Msg msg) in
  let ( let* ) = Result.bind in
  let* chip = chip_open_by_name (Some "gpiochip0") |> t "couldn't open chip" in
  let* line = chip_get_line (Some chip) line |> t "couldn't get line" in
  let* () =
    line_request_output (Some line) (Some "example") v
    |> nz "couldn't request line"
  in
  let* () = line_set_value (Some line) v |> nz "couldn't set value" in
  chip_close (Some chip);
  Result.ok ()

let write m x =
  let v = match x with High -> 1 | Low -> 0 in
  match gpiod_write (Mode.get_pin m) v with
  | Ok () -> ()
  | Error (`Msg msg) -> Printf.printf "Couldn't set line: %s!\n%!" msg

let reset () =
  write Mode.Active Low;
  write Mode.Disabled Low;
  write Mode.Idle Low

let select m =
  (* Printf.printf "Switching to mode %s\n%!" (Mode.show m) ; *)
  reset ();
  write m High

let init () = select Mode.Disabled
let ( let* ) = Lwt.bind

let sleep ?blink_mode s =
  match blink_mode with
  | None -> Lwt_unix.sleep s
  | Some mode ->
      let rec aux left =
        if left < 1. then Lwt_unix.sleep left
        else (
          write mode High;
          let* _ = Lwt_unix.sleep 0.1 in
          write mode Low;
          let* _ = Lwt_unix.sleep 0.9 in
          aux (left -. 1.))
      in
      aux s
