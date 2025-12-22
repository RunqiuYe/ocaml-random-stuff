open! Core
open Async

let launch ~port ~uppercase =
  let prog = "../bin/echo.exe" in
  let args =
    [ "-port"; Int.to_string port ] @ if uppercase then [ "-uppercase" ] else []
  in
  let%map process_or_error = Process.create ~prog ~args () in
  Or_error.ok_exn process_or_error

let rec connect ~port =
  let where =
    Tcp.Where_to_connect.of_host_and_port
      { Host_and_port.host = "localhost"; port }
  in
  match%bind Monitor.try_with (fun () -> Tcp.connect where) with 
  | Ok (_, r, w) -> return (r, w)
  | Error _ -> 
    let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.01) in 
    connect ~port

let send_data r w msg =
  Writer.write w msg;
  let%bind () = Writer.flushed w in
  let%bind response = Reader.read_line r in
  match response with
  | `Eof -> failwith "unexpected EOF from server"
  | `Ok line ->
      print_endline line;
      return ()

let cleanup process =
  let pid = Process.pid process in
  ignore (Signal_unix.send Signal.term (`Pid pid));
  let%bind (_ : Unix.Exit_or_signal.t) = Process.wait process in
  return ()
