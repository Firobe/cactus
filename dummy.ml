module IO : Signatures.IO = struct
  type t = unit

  let init () = Result.ok ()
  let select () _ = ()

  let sleep () ?blink_mode:_ ?blink_interval:_ ?blink_duration:_ v =
    Lwt_unix.sleep v

  let reset () = ()
end

module Temperature : Signatures.Temperature = struct
  type t = unit

  let init () = ()
  let get () = Lwt.return 27.13
  let adjust _ _ = Lwt.return 0.
  let offset _ = 0.
end
