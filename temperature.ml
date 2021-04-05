open Stdint

type calibration =
  { ac1: Int16.t
  ; ac2: Int16.t
  ; ac3: Int16.t
  ; ac4: Uint16.t
  ; ac5: Uint16.t
  ; ac6: Uint16.t
  ; b1: Int16.t
  ; b2: Int16.t
  ; mb: Int16.t
  ; mc: Int16.t
  ; md: Int16.t }

type t = {i2c: I2c.t; cal: calibration; mutable offset: float}

let get_ok = function Ok x -> x | Error _ -> failwith "Couldn't operate"

(* TODO read_word with correct endianness *)

let read_word_b i2c n =
  let msb = I2c.read_byte_data i2c (Uint8.of_int n) |> get_ok in
  let lsb = I2c.read_byte_data i2c (Uint8.of_int (n + 1)) |> get_ok in
  Uint16.(shift_left (of_uint8 msb) 8 + of_uint8 lsb)

let calibrate i2c =
  let r = read_word_b i2c in
  let ac1 = r 0xAA |> Int16.of_uint16 in
  let ac2 = r 0xAC |> Int16.of_uint16 in
  let ac3 = r 0xAE |> Int16.of_uint16 in
  let ac4 = r 0xB0 in
  let ac5 = r 0xB2 in
  let ac6 = r 0xB4 in
  let b1 = r 0xB6 |> Int16.of_uint16 in
  let b2 = r 0xB8 |> Int16.of_uint16 in
  let mb = r 0xBA |> Int16.of_uint16 in
  let mc = r 0xBC |> Int16.of_uint16 in
  let md = r 0xBE |> Int16.of_uint16 in
  {ac1; ac2; ac3; ac4; ac5; ac6; b1; b2; mb; mc; md}

let show_cal cal =
  let open Int16 in
  Printf.sprintf
    "AC1   %s\n\
     AC2   %s\n\
     AC3   %s\n\
     AC4   %s\n\
     AC5   %s\n\
     AC6   %s\n\
     B1    %s\n\
     B2    %s\n\
     MB    %s\n\
     MC    %s\n\
     MD    %s" (to_string_hex cal.ac1) (to_string_hex cal.ac2)
    (to_string_hex cal.ac3)
    (Uint16.to_string_hex cal.ac4)
    (Uint16.to_string_hex cal.ac5)
    (Uint16.to_string_hex cal.ac6)
    (to_string_hex cal.b1) (to_string_hex cal.b2) (to_string_hex cal.mb)
    (to_string_hex cal.mc) (to_string_hex cal.md)

let get_new_raw_temperature i2c =
  assert (
    I2c.write_byte_data i2c (Uint8.of_int 0xF4) (Uint8.of_int 0x2E)
    |> get_ok = () ) ;
  WiringPi.delay 6 ;
  read_word_b i2c 0xF6

let get_without_offset t = 
  let raw = Uint16.to_int (get_new_raw_temperature t.i2c) in
  let x1 = ((raw - Uint16.to_int t.cal.ac6) * Uint16.to_int t.cal.ac5) asr 15 in
  let x2 = (Int16.to_int t.cal.mc lsl 11) / (x1 + Int16.to_int t.cal.md) in
  (* TODO for pressure, update b6 *)
  let temp = (x1 + x2 + 8) asr 4 in
  float_of_int temp /. 10.

let adjust cal t =
  let temp = get_without_offset t in
  let off = cal -. temp in
  t.offset <- off ;
  off

let get t =
  let raw = get_without_offset t in
  raw +. t.offset

let init () =
  let i2c_address = 0x77 in
  let fd = Unix.(openfile "/dev/i2c-1" [O_RDWR] 0x644) in
  let i2c =
    match I2c.set_address fd i2c_address with
    | Result.Ok x -> x
    | Result.Error _ -> failwith "Cannot access to temperature sensor" in
  let cal = calibrate i2c in
  Printf.printf "Calibration OK\n" ;
  {i2c; cal; offset = 0.}
