type t = Active | Idle | Disabled [@@deriving show]

let of_string = function
  | "Active" | "active" | "1" | "on" -> Active
  | "Idle" | "idle" -> Idle
  | "Disabled" | "disabled" | "off" | "0" -> Disabled
  | s -> failwith ("Wrong mode: " ^ s)

let get_pin = function Active -> 1 | Idle -> 2 | Disabled -> 3
