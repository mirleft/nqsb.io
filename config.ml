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

let logger =
  syslog_udp
    (syslog_config ~truncate:1484 "nqsb.io" (Ipaddr.V4.of_string_exn "198.167.222.206"))
    net

let () =
  let packages = [
    package ~sublibs:["mirage"] "tls";
    package "tyxml";
    package ~sublibs:["lwt"] "logs"
  ] in
  register "nqsb.io" [
    foreign
      ~deps:[abstract nocrypto; abstract logger]
      ~packages
      "Unikernel.Main"
      (stackv4 @-> kv_ro @-> kv_ro @-> job)
    $ net
    $ crunch "tls"
    $ crunch "disk"
  ]
