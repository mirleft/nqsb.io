open Mirage

let net = generic_stackv4 default_network

let logger = syslog_udp ~config:(syslog_config "nqsb.io") net

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

let keys = Key.[
    abstract dns_key ; abstract dns_server ; abstract dns_port ;
    abstract key_seed
  ]

let () =
  let packages = [
    package ~sublibs:["mirage"] "tls";
    package "tyxml";
    package "logs" ;
    package "dns-mirage-certify" ;
    package ~min:"0.2.1" "logs-syslog" ;
    package ~min:"3.7.1" "tcpip" ;
    package ~min:"2.0.0" "mirage-kv" ;
    package "monitoring-experiments" ;
  ] in
  register "nqsb.io" [
    foreign
      ~deps:[ abstract nocrypto ; abstract logger ; abstract app_info ]
      ~keys
      ~packages
      "Unikernel.Main"
      (random @-> pclock @-> mclock @-> time @-> stackv4 @-> kv_ro @-> kv_ro @-> job)
    $ default_random $ default_posix_clock $ default_monotonic_clock $ default_time $ net $ crunch "tls" $ crunch "disk"
  ]
