open Grpc_lwt

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

let server =
  Server.(
    v () |> add_service ~name:"mypackage.Greeter" ~service:greeter_service)

let () =
  let open Lwt.Syntax in
  let port = 8081 in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
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
      let server =
        let certfile = "./certs/server.crt" in
        let keyfile = "./certs/server.pem" in
        H2_lwt_unix.Server.TLS.create_connection_handler_with_default ~certfile
          ~keyfile ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler
      in
      let+ _server =
        Lwt_io.establish_server_with_client_socket listen_address server
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