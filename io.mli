type t

val init : unit -> (t, [ `Msg of string ]) Result.t
val select : t -> Mode.t -> unit
val sleep : t -> ?blink_mode:Mode.t -> float -> unit Lwt.t
val reset : t -> unit
