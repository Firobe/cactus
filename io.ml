type signal = Low | High

let write m x =
  let v = match x with High -> 1 | Low -> 0 in
  WiringPi.digitalWrite (Mode.get_pin m) v

let reset () =
  write Mode.Active Low ; write Mode.Disabled Low ; write Mode.Idle Low

let select m =
  Printf.printf "Switching to mode %s\n%!" (Mode.show m) ;
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
  let* () = Lwt.pause () in
  let fsec_to_millis s = int_of_float @@ (s *. 1000.) in
  let ms = fsec_to_millis s in
  match blink_mode with
  | None -> WiringPi.delay ms ; Lwt.return_unit
  | Some mode ->
      let rec aux left =
        if left < 1000 then (WiringPi.delay left ; Lwt.return_unit)
        else
          let* _ = Lwt.pause () in
          write mode High ;
          WiringPi.delay 100 ;
          write mode Low ;
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          let* _ = Lwt.pause () in
          WiringPi.delay 100 ;
          aux (left - 1000) in
      aux ms
