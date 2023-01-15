val init : unit -> unit
val select : Mode.t -> unit
val sleep : ?blink_mode:Mode.t -> float -> unit Lwt.t
val reset : unit -> unit
