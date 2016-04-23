open Lwt.Infix
open V1_LWT

module Main (C : CONSOLE) (S : STACKV4) (KEYS : KV_RO) (KV : KV_RO) (Clock : V1.CLOCK) =
struct
  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)

  let read_key kv name =
    KEYS.size kv name >>= function
    | `Error (KEYS.Unknown_key e) -> Lwt.fail (Invalid_argument e)
    | `Ok size ->
      KEYS.read kv name 0 (Int64.to_int size) >>= function
      | `Error (KEYS.Unknown_key e) -> Lwt.fail (Invalid_argument e)
      | `Ok cs -> Lwt.return (Cstruct.concat cs)

  let read_cert kv name =
    read_key kv (name ^ ".pem") >>= fun chain ->
    read_key kv (name ^ ".key") >|= fun key ->
    let open X509.Encoding.Pem in
    (Certificate.of_pem_cstruct chain,
     match Private_key.of_pem_cstruct1 key with
     | `RSA key -> key)

  let log c tag (ip, port) msg =
    C.log c (Printf.sprintf "%.04f [%s] %s:%d %s"
               (Clock.time ()) tag (Ipaddr.V4.to_string ip) port  msg)

  let http_header ~status xs =
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

  let header content_type =
    http_header
      ~status:"HTTP/1.1 200 OK"
      [ ("content-type", content_type) ;
        ("Strict-Transport-Security", "max-age=31536000; includeSubDomains") ]

  let read_kv kv name =
    KV.size kv name >>= function
    | `Error (KV.Unknown_key e) -> Lwt.fail (Invalid_argument e)
    | `Ok size ->
      KV.read kv name 0 (Int64.to_int size) >>= function
      | `Error (KV.Unknown_key e) -> Lwt.fail (Invalid_argument e)
      | `Ok bufs -> Lwt.return (Cstruct.concat bufs)

  let read_pdf kv name =
    read_kv kv name >|= fun data ->
    [ header "application/pdf" ; data ]

  let tls_accept ~tag ~f c cfg tcp =
    let peer = TCP.get_dest tcp in
    let log  = log c tag peer in
    let with_tls_server k = TLS.server_of_flow cfg tcp >>= k
    in
    with_tls_server @@ function
    | `Error e -> log ("TLS failed" ^ TLS.error_message e) ; TCP.close tcp
    | `Eof     -> log "TLS eof" ; TCP.close tcp
    | `Ok tls  -> TLS.writev tls (f log tls) >>= fun _ -> TLS.close tls

  let moved_permanently = http_header
      ~status:"HTTP/1.1 301 Moved permanently"
      [ ("location", "https://nqsb.io") ]

  let h_notice c =
    fun tcp ->
      let log = log c "web" (TCP.get_dest tcp) in
      TCP.write tcp moved_permanently >>= function
      | `Error _ -> log "write error" ; TCP.close tcp
      | _        -> log "responded"   ; TCP.close tcp

  let dispatch nqsb usenix tron log tls =
    match TLS.epoch tls with
    | `Error -> log "error while getting epoch" ; nqsb
    | `Ok e ->
      match e.Tls.Core.own_name with
      | Some "usenix15.nqsb.io" -> log "serving usenix" ; usenix
      | Some "tron.nqsb.io" ->  log "serving tron" ; tron
      | Some "nqsb.io" ->  log "serving nqsb.io" ; nqsb
      | Some x -> log ("SNI is " ^ x) ; nqsb
      | None -> log "no sni" ; nqsb

  let start con stack keys kv _clock _ =
    let d_nqsb = [ header "text/html;charset=utf-8" ; Page.render ] in
    read_pdf kv "nqsbtls-usenix-security15.pdf" >>= fun d_usenix ->
    read_pdf kv "tron.pdf" >>= fun d_tron ->
    let f = dispatch d_nqsb d_usenix d_tron in

    read_cert keys "nqsb" >>= fun c_nqsb ->
    read_cert keys "usenix15" >>= fun c_usenix ->
    read_cert keys "tron" >>= fun c_tron ->
    let config = Tls.Config.server ~certificates:(`Multiple_default (c_nqsb, [ c_usenix ; c_tron])) () in

    S.listen_tcpv4 stack ~port:80 (h_notice con) ;
    S.listen_tcpv4 stack ~port:443 (tls_accept ~tag:"web-server" ~f con config) ;
    S.listen stack
end
