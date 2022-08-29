open Mirage

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt (some ip_address) None doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt (some ip_address) None doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "nqsb.io" doc))

let keys = [
  Key.v name ; Key.v monitor ; Key.v syslog
]

let net = generic_stackv4v6 default_network

let management_stack = generic_stackv4v6 ~group:"management" (netif ~group:"management" "management")

let () =
  let packages = [
    package "tyxml";
    package "logs" ;
    package ~sublibs:["mirage"] ~min:"0.3.0" "logs-syslog" ;
    package ~min:"3.7.1" "tcpip" ;
    package ~min:"2.0.0" "mirage-kv" ;
    package "mirage-monitoring" ;
  ] in
  register "nqsbio" [
    foreign
      ~keys
      ~packages
      "Unikernel.Main"
      (console @-> time @-> pclock @-> stackv4v6 @-> kv_ro @-> stackv4v6 @-> job)
    $ default_console $ default_time $ default_posix_clock $ net $ crunch "disk" $ management_stack
  ]
