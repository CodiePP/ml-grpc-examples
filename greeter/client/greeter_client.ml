open Grpc_lwt
open Lwt.Syntax

let server_port = 8081
let server_address = "localhost"
let client_address = "localhost"
let cacertfile = "./certs/ca.crt"
let certfile = "./certs/client.crt"
let keyfile = "./certs/client.pem"

let call_server req =
  let* addresses =
    Lwt_unix.getaddrinfo server_address (string_of_int server_port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let* certificate = X509_lwt.private_of_pems ~cert:certfile ~priv_key:keyfile in
  (* make sure server's certificate is signed by this CA *)
  let* authenticator = X509_lwt.authenticator (`Ca_file cacertfile) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  let* connection =
    (* H2_lwt_unix.Client.TLS.create_connection_with_default ~error_handler socket *)
    H2_lwt_unix.Client.TLS.create_connection_full
    ~error_handler
    ~certificates:(`Single certificate)
    ~peer_name:(Domain_name.of_string_exn client_address |> Domain_name.host_exn)
    ?ciphers:None
    ?version:(Some (`TLS_1_3, `TLS_1_3))
    ?signature_algorithms:None
    ?reneg:(Some true)
    ~authenticator:authenticator
    ?ip:None
    socket
  in
  (* code generation *)
  let enc = Pbrt.Encoder.create () in
  Greeter_pb.encode_greet_request req enc;

  Client.call ~service:"ml_grpc_examples.Greeter" ~rpc:"Greet"
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
  let first_name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "anonymous" in
  let req = Greeter_pb__Greeter_types.default_greet_request ~first_name () in
  Lwt_main.run
    (let+ res = call_server req in
    match res with
    | Ok (res, _) -> print_endline res.result
    | Error _ -> print_endline "an error occurred")