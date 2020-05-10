open Mirage

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"dns server IP" ["dns-server"] in
  Key.(create "dns-server" Arg.(required ipv4_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"dns server port" ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"certificate key seed" ["key-seed"] in
  Key.(create "key-seed" Arg.(required string doc))

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "nqsb.io" doc))

let keys = [
  Key.abstract dns_key ; Key.abstract dns_server ; Key.abstract dns_port ; Key.abstract key_seed ;
  Key.abstract name ; Key.abstract monitor ; Key.abstract syslog
  ]

let net = generic_stackv4 default_network

let management_stack = generic_stackv4 ~group:"management" (netif ~group:"management" "management")

let () =
  let packages = [
    package "tls-mirage";
    package "tyxml";
    package "logs" ;
    package ~sublibs:["mirage"] "dns-certify" ;
    package ~sublibs:["mirage"] ~min:"0.2.1" "logs-syslog" ;
    package ~min:"3.7.1" "tcpip" ;
    package ~min:"2.0.0" "mirage-kv" ;
    package "monitoring-experiments" ;
  ] in
  register "nqsb.io" [
    foreign
      ~keys
      ~packages
      "Unikernel.Main"
      (console @-> random @-> time @-> mclock @-> pclock @-> stackv4 @-> kv_ro @-> stackv4 @-> job)
    $ default_console $ default_random $ default_time $ default_monotonic_clock $ default_posix_clock $ net $ crunch "disk" $ management_stack
  ]
