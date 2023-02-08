open Grpc_lwt
open Lwt.Syntax

let call_server address port req =
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  let* connection =
    H2_lwt_unix.Client.TLS.create_connection_with_default ~error_handler socket
  in
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Greeter_pb.encode_greet_request req enc;

  Client.call ~service:"mypackage.Greeter" ~rpc:"Greet"
    ~do_request:(H2_lwt_unix.Client.TLS.request connection ~error_handler:ignore)
    ~handler:
      (Client.Rpc.unary (Pbrt.Encoder.to_string enc) ~f:(fun decoder ->
          let+ decoder = decoder in
          match decoder with
          | Some decoder ->
              let decoder = Pbrt.Decoder.of_string decoder in
              Greeter_pb.decode_greet_response decoder
          | None -> Greeter_pb__Greeter_types.default_greet_response ()))
    ()

let () =
  let open Lwt.Syntax in
  let port = 8081 in
  let address = "localhost" in
  let first_name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let req = Greeter_pb__Greeter_types.default_greet_request ~first_name () in
  Lwt_main.run
    (let+ res = call_server address port req in
    match res with
    | Ok (res, _) -> print_endline res.result
    | Error _ -> print_endline "an error occurred")