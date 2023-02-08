# Examples of gRPC in OCaml

## test greeter

in one terminal:
`dune exec -- greeter/server/greeter_server.exe`

in another terminal:
`dune exec -- greeter/client/greeter_client.exe world`


output:

```
Hello, world!
```

