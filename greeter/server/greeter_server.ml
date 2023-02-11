open Grpc_lwt
open Lwt.Syntax

let cacertfile = "./certs/ca.crt"
let certfile = "./certs/server.crt"
let keyfile = "./certs/server.pem"
let port = 8081


let say_hello buffer =
  let decoder = Pbrt.Decoder.of_string buffer in
  let req = Greeter_pb.decode_greet_request decoder in
  let result =
    if req.first_name = "" then "You forgot your name!"
    else Format.sprintf "Hello, %s!" req.first_name
  in
  let reply =  Greeter_pb__Greeter_types.default_greet_response ~result () in
  let encoder = Pbrt.Encoder.create () in
  Greeter_pb.encode_greet_response reply encoder;
  Lwt.return (Grpc.Status.(v OK), Some (Pbrt.Encoder.to_string encoder))

let greeter_service =
  Server.Service.(
    v () |> add_rpc ~name:"Greet" ~rpc:(Unary say_hello) |> handle_request)

let greeter_server =
  Server.(
    v () |> add_service ~name:"ml_grpc_examples.Greeter" ~service:greeter_service)

let () =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      (* make sure that client certificates are signed by this CA *)
      let* authenticator = X509_lwt.authenticator (`Ca_file cacertfile) in
      let* certificate = X509_lwt.private_of_pems ~cert:certfile ~priv_key:keyfile in
      let error_handler :
          Unix.sockaddr ->
          ?request:H2.Request.t ->
          _ ->
          (H2.Headers.t -> H2.Body.Writer.t) ->
          unit =
        fun _client_address ?request:_ _error start_response ->
          let response_body = start_response H2.Headers.empty in
          (* begin match error with | `Exn exn -> Body.write_string response_body
            (Printexc.to_string exn); Body.write_string response_body "\n";

            | #Status.standard as error -> Body.write_string response_body
            (Status.default_reason_phrase error) end; *)
          H2.Body.Writer.close response_body
      in
      let sockaddr2s = function
            Unix.ADDR_UNIX fn -> fn
          | Unix.ADDR_INET (ip, port) -> Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
      in
      let server client_address client_socket  =
        let* () = Lwt_io.printlf "new connection from %s" (sockaddr2s client_address) in
        try
          H2_lwt_unix.Server.TLS.create_connection_handler_full
            ~certificates:(`Single certificate)
            ?config:None
            ~request_handler:(fun _ reqd -> Server.handle_request greeter_server reqd)
            ~error_handler
            ?ciphers:(Some [`CHACHA20_POLY1305_SHA256;`AES_256_GCM_SHA384])
            ?version:(Some (`TLS_1_3, `TLS_1_3)) (* only TLS 1.3 *)
            ?signature_algorithms:None
            ?reneg:(Some false)
            ?acceptable_cas:None
            ?authenticator:(Some authenticator)
            ?zero_rtt:(Some 0l)
            ?ip:None
            client_address client_socket
        with failure ->
          Lwt_io.printlf "Failure: %s" (Printexc.to_string failure)
      in
      let+ _server =
        Lwt_io.establish_server_with_client_socket ?no_close:(Some false) ?backlog:(Some 12) listen_address server
      in
      Printf.printf "Listening on port %i for grpc requests\n" port;
      print_endline "";
      print_endline "Try running:";
      print_endline "";
      print_endline
        {| dune exec -- greeter/client/greeter_client.exe <your_name> |}
    );

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever