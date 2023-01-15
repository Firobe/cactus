type t

val init : unit -> t
val get : t -> float Lwt.t
val adjust : float -> t -> float Lwt.t
val offset : t -> float
