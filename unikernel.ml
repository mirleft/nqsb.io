open Lwt.Infix
open V1_LWT

module Main (C : CONSOLE) (CLOCK: V1.PCLOCK) (S : STACKV4) (KEYS : KV_RO) (KV : KV_RO) =
struct
  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)
  module UDPLOG = Logs_syslog_mirage.Udp(C)(CLOCK)(S.UDPV4)

  let read_key kv name =
    KEYS.size kv name >>= function
    | `Error _ -> Lwt.fail (invalid_arg "error")
    | `Ok size ->
      KEYS.read kv name 0 (Int64.to_int size) >>= function
      | `Error _ -> Lwt.fail (invalid_arg "error")
      | `Ok cs -> Lwt.return (Cstruct.concat cs)

  let read_cert kv name =
    read_key kv (name ^ ".pem") >>= fun chain ->
    read_key kv (name ^ ".key") >|= fun key ->
    let open X509.Encoding.Pem in
    (Certificate.of_pem_cstruct chain,
     match Private_key.of_pem_cstruct1 key with
     | `RSA key -> key)

  let log tag (ip, port) msg =
    Logs_lwt.info (fun m -> m "[%s] %s:%d %s" tag (Ipaddr.V4.to_string ip) port msg)

  let http_header ~status xs =
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

  let header content_type =
    http_header
      ~status:"HTTP/1.1 200 OK"
      [ ("Content-Type", content_type) ;
        ("Strict-Transport-Security", "max-age=31536000; includeSubDomains") ;
        ("Connection", "close") ]

  let read_kv kv name =
    KV.size kv name >>= function
    | `Error e -> Lwt.fail (invalid_arg "failed")
    | `Ok size ->
      KV.read kv name 0 (Int64.to_int size) >>= function
      | `Error e -> Lwt.fail (invalid_arg "failed")
      | `Ok bufs -> Lwt.return (Cstruct.concat bufs)

  let read_pdf kv name =
    read_kv kv name >|= fun data ->
    [ header "application/pdf" ; data ]

  let tls_accept ~tag ~f cfg tcp =
    let peer = TCP.dst tcp in
    let log  = log tag peer in
    let with_tls_server k = TLS.server_of_flow cfg tcp >>= k
    in
    with_tls_server @@ function
    | Error _ -> log "TLS failed" >>= fun () -> TCP.close tcp
    | Ok tls ->
      f log tls >>= fun data ->
      TLS.writev tls data >>= fun _ ->
      TLS.close tls

  let moved_permanently = http_header
      ~status:"HTTP/1.1 301 Moved permanently"
      [ ("location", "https://nqsb.io") ]

  let h_notice ~tag =
    fun tcp ->
      let log = log tag (TCP.dst tcp) in
      TCP.write tcp moved_permanently >>= function
      | Error `Closed -> log "write error, closed"
      | Error (`Msg str) -> log ("write error" ^ str)
      | Ok () -> log "responded" >>= fun () -> TCP.close tcp

  let dispatch nqsb usenix tron log tls =
    match TLS.epoch tls with
    | Error _ -> log "error while getting epoch, serving nqsb.io" >|= fun () -> nqsb
    | Ok e ->
      match e.Tls.Core.own_name with
      | Some "usenix15.nqsb.io" -> log "serving usenix pdf" >|= fun () -> usenix
      | Some "tron.nqsb.io" ->  log "serving tron pdf" >|= fun () -> tron
      | Some "nqsb.io" ->  log "serving nqsb.io" >|= fun () -> nqsb
      | Some x -> log ("SNI is " ^ x ^ ", servion nqsb.io")  >|= fun () -> nqsb
      | None -> log "no sni, serving nqsb.io" >|= fun () -> nqsb

  let start con clock stack keys kv _ =
    let d_nqsb = [ header "text/html;charset=utf-8" ; Page.render ] in
    read_pdf kv "nqsbtls-usenix-security15.pdf" >>= fun d_usenix ->
    read_pdf kv "tron.pdf" >>= fun d_tron ->
    let f = dispatch d_nqsb d_usenix d_tron in

    read_cert keys "nqsb" >>= fun c_nqsb ->
    read_cert keys "usenix15" >>= fun c_usenix ->
    read_cert keys "tron" >>= fun c_tron ->
    let config = Tls.Config.server ~certificates:(`Multiple_default (c_nqsb, [ c_usenix ; c_tron])) () in

    let reporter =
      let ip = Ipaddr.V4.of_string_exn "198.167.222.206" in
      UDPLOG.create con clock (S.udpv4 stack) ~hostname:"web.nqsb.io" ip ()
    in
    Logs.set_reporter reporter ;

    S.listen_tcpv4 stack ~port:80 (h_notice ~tag:"HTTP") ;
    S.listen_tcpv4 stack ~port:443 (tls_accept ~tag:"HTTPS" ~f config) ;
    S.listen stack
end
