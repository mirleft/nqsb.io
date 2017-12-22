open Mirage

let net =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~arp:farp default_network)

let logger = syslog_udp ~config:(syslog_config ~truncate:1484 "nqsb.io") net

let () =
  let packages = [
    package ~sublibs:["mirage"] "tls";
    package "tyxml";
    package "logs"
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
