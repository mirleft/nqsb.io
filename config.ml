open Mirage

let address addr nm gw =
  let f = Ipaddr.V4.of_string_exn in
  { address = f addr ; netmask = f nm ; gateways = [f gw] }

let server = address "198.167.222.201" "255.255.255.0" "198.167.222.1"

let net =
  match get_mode () with
  | `Unix -> socket_stackv4 default_console [Ipaddr.V4.any]
  | `Xen  -> direct_stackv4_with_static_ipv4 default_console tap0 server


let kv = crunch "disk"

let () =
  let packages = ["tls"; "tcpip"; "tyxml"]
  and libraries = ["tls.mirage"; "tyxml"]
  in
  register "nqsb.io" [
    foreign
      ~deps:[abstract nocrypto]
      ~libraries
      ~packages
      "Unikernel.Main"
      ( console @-> stackv4 @-> kv_ro @-> clock @-> kv_ro @-> job )
    $ default_console
    $ net
    $ kv
    $ default_clock
    $ crunch "tls"
  ]
