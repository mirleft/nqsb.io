open Lwt.Infix
open V1
open V1_LWT

module Http = struct
  let headers status xs =
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines   = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)
end


module Main (C : CONSOLE) (S : STACKV4) (KV : KV_RO) (Clock : CLOCK) (KEYS : KV_RO) =
struct

  module TCP  = S.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (KEYS) (Clock)

  let log c tag (ip, port) msg =
    let pre = Printf.sprintf "[%s] %s:%d " tag (Ipaddr.V4.to_string ip) port in
    let data = Printf.sprintf "%s%.04f %s\n" pre (Clock.time ()) msg in
    C.log c data

  let read_kv kv name =
    let file = "assets" ^ name in
    KV.size kv file
    >>= function
      | `Error (KV.Unknown_key _) -> Lwt.fail (Invalid_argument name)
      | `Ok size ->
         KV.read kv file 0 (Int64.to_int size)
         >>= function
           | `Error (KV.Unknown_key _) -> Lwt.fail (Invalid_argument name)
           | `Ok bufs -> Lwt.return (Cstruct.concat bufs)

  let content_type path =
    try
      let idx = String.index path '.' + 1 in
      let rt = String.sub path idx (String.length path - idx) in
      match rt with
      | "pdf" -> "application/pdf"
      | _ -> "text/plain"
    with _ -> "text/plain"

  let http_header ~status xs =
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines   = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

  let header content_type = http_header
      ~status:"HTTP/1.1 200 OK"
      [ ("content-type", content_type) ;
        ("Strict-Transport-Security", "max-age=31536000; includeSubDomains") ]

  let cut_line str =
    match Astring.String.cut ~sep:"GET " str with
    | None -> None
    | Some (_, right) -> match Astring.String.cut ~sep:" HTTP" right with
      | None -> None
      | Some (left, _) -> Some left

  let err = http_header
      ~status:"HTTP/1.1 404 Not found" [ ("content-type", "text/plain") ]

  let dispatch _c kv data tls =
    Lwt.catch (fun () ->
      TLS.read tls >>= function
      | `Ok req ->
        (match cut_line (Cstruct.to_string req) with
         | None -> invalid_arg "couldn't parse request"
         | Some "/" -> TLS.writev tls [ header "text/html; charset=UTF-8"; data ]
         | Some x when x = "/nqsbtls-usenix-security15.pdf" ->
           read_kv kv x >>= fun data ->
           let ct = content_type x in
           TLS.writev tls [ header ct; data ]
         | Some _ -> invalid_arg "unknown resource")
      | _ -> invalid_arg "couldn't read")
      (fun _e ->
        TLS.writev tls [ err ; Cstruct.of_string "Not found" ])

  let tls_accept ~tag c cfg tcp ~f =
    let peer = TCP.get_dest tcp in
    let log  = log c tag peer in
    let with_tls_server k = TLS.server_of_flow cfg tcp >>= k
    in
    with_tls_server @@ function
    | `Error _ -> log "TLS failed" ; TCP.close tcp
    | `Eof     -> log "TLS eof"    ; TCP.close tcp
    | `Ok tls  -> log "TLS ok"     ; f tls >>= fun _ -> TLS.close tls


  let moved_permanently = http_header
      ~status:"HTTP/1.1 301 Moved permanently"
      [ ("location", "https://nqsb.io") ]

  let h_notice c =
    fun tcp ->
      let log = log c "web" (TCP.get_dest tcp) in
      TCP.write tcp moved_permanently >>= function
      | `Error _ -> log "write error" ; TCP.close tcp
      | _        -> log "responded"   ; TCP.close tcp

  let h_as_web_server c kv data cfg =
    tls_accept ~tag:"web-server" c cfg ~f:(dispatch c kv data)

  let start con stack kv _clock keys _ =
    X509.certificate keys (`Name "nqsb") >>= fun cert ->
    let config = Tls.Config.server ~certificates:(`Single cert) ()
    and web_data = Page.render
    in
    S.listen_tcpv4 stack ~port:80 (h_notice con) ;
    S.listen_tcpv4 stack ~port:443 (h_as_web_server con kv web_data config) ;
    S.listen stack

end
