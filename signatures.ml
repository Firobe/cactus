module type IO = sig
  type t

  val init : unit -> (t, [ `Msg of string ]) Result.t
  val select : t -> Mode.t -> unit
  val sleep : t -> ?blink_mode:Mode.t -> float -> unit Lwt.t
  val reset : t -> unit
end

module type Temperature = sig
  type t

  val init : unit -> t
  val get : t -> float Lwt.t
  val adjust : float -> t -> float Lwt.t
  val offset : t -> float
end

type state = { on : bool; goal : float }

module type Server = functor (Temp : Temperature) -> sig
  val init : (string * string) option -> float -> Temp.t -> unit Lwt.t
  val state : unit -> state
end
