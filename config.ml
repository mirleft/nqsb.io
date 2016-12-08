open Mirage

let address =
  let network = Ipaddr.V4.Prefix.of_address_string_exn "198.167.222.201/24"
  and gateway = Ipaddr.V4.of_string "198.167.222.1"
  in
  { network ; gateway }

let net =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~config:address default_network)


let () =
  let packages = [package ~sublibs:["mirage"] "tls"; package "tyxml"]
  in
  register "nqsb.io" [
    foreign
      ~deps:[abstract nocrypto]
      ~packages
      "Unikernel.Main"
      ( console @-> stackv4 @-> kv_ro @-> kv_ro @-> job )
    $ default_console
    $ net
    $ crunch "tls"
    $ crunch "disk"
  ]
