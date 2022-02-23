type signal = Low | High

let write m x =
  let v = match x with High -> 1 | Low -> 0 in
  WiringPi.digitalWrite (Mode.get_pin m) v

let reset () =
  write Mode.Active Low ; write Mode.Disabled Low ; write Mode.Idle Low

let select m =
  (* Printf.printf "Switching to mode %s\n%!" (Mode.show m) ; *)
  reset () ;
  write m High

let init () =
  let open WiringPi in
  ignore (setup ()) ;
  pinMode Mode.(get_pin Active) 1 ;
  pinMode Mode.(get_pin Disabled) 1 ;
  pinMode Mode.(get_pin Idle) 1 ;
  select Mode.Disabled

let ( let* ) = Lwt.bind

let sleep ?blink_mode s =
  match blink_mode with
  | None -> Lwt_unix.sleep s
  | Some mode ->
    let rec aux left =
      if left < 1. then Lwt_unix.sleep left
      else begin
        write mode High ;
        let* _ = Lwt_unix.sleep 0.1 in
        write mode Low ;
        let* _ = Lwt_unix.sleep 0.9 in
        aux (left -. 1.)
      end in aux s
