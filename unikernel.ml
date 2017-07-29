open Lwt.Infix
open Mirage_types_lwt

module Main (S : STACKV4) (KEYS : KV_RO) (KV : KV_RO) =
struct
  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)

  let read_key kv name =
    KEYS.size kv name >>= function
    | Error e ->
      Logs.warn (fun m -> m "keys: error while calling size %s: %a" name KEYS.pp_error e);
      Lwt.fail (invalid_arg "error")
    | Ok size ->
      KEYS.read kv name 0L size >>= function
      | Error e ->
        Logs.warn (fun m -> m "keys: error while calling read %s: %a" name KEYS.pp_error e);
        Lwt.fail (invalid_arg "error")
      | Ok cs -> Lwt.return (Cstruct.concat cs)

  let read_cert kv name =
    read_key kv (name ^ ".pem") >>= fun chain ->
    read_key kv (name ^ ".key") >|= fun key ->
    let open X509.Encoding.Pem in
    (Certificate.of_pem_cstruct chain,
     match Private_key.of_pem_cstruct1 key with
     | `RSA key -> key)

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
    | Error e ->
      Logs.warn (fun m -> m "kv: error while calling size %s: %a" name KV.pp_error e);
      Lwt.fail (invalid_arg "failed")
    | Ok size ->
      KV.read kv name 0L size >>= function
      | Error e ->
        Logs.warn (fun m -> m "kv: error while calling read %s: %a" name KV.pp_error e);
        Lwt.fail (invalid_arg "failed")
      | Ok bufs -> Lwt.return (Cstruct.concat bufs)

  let read_pdf kv name =
    read_kv kv name >|= fun data ->
    [ header "application/pdf" ; data ]

  let stored_tags = Lwt.new_key ()

  let add_tag ntag nval =
    let other = match Lwt.get stored_tags with
      | None -> Logs.Tag.empty
      | Some x -> x
    in
    Logs.Tag.add ntag nval other

  let with_tag ntag nval f =
    Lwt.with_value stored_tags (Some (add_tag ntag nval)) f

  let peer_tag : (Ipaddr.V4.t * int) Logs.Tag.def =
    Logs.Tag.def "peer" ~doc:"connection endpoint"
      Fmt.(pair ~sep:(unit ":") Ipaddr.V4.pp_hum int)

  let tls_tag : Tls.Core.epoch_data Logs.Tag.def =
    Logs.Tag.def "tls" ~doc:"TLS parameters"
      (fun ppf epoch ->
         let open Tls.Core in
         let open Sexplib.Sexp in
         Fmt.pf ppf "%a,%a,%a,extended_ms=%a"
           pp (sexp_of_tls_version epoch.protocol_version)
           pp (Tls.Ciphersuite.sexp_of_ciphersuite epoch.ciphersuite)
           Fmt.(option ~none:(unit "no SNI") string) epoch.own_name
           Fmt.bool epoch.extended_ms)

  let tls_accept ~f cfg tcp =
    with_tag peer_tag (TCP.dst tcp)
      (fun () ->
         TLS.server_of_flow cfg tcp >>= function
         | Error e ->
           Logs.warn (fun m -> m ?tags:(Lwt.get stored_tags) "TLS error %a"
                         TLS.pp_write_error e);
           TCP.close tcp
         | Ok tls ->
           let data = f tls in
           TLS.writev tls data >>= fun _ ->
           TLS.close tls)

  let moved_permanently =
    http_header ~status:"HTTP/1.1 301 Moved permanently"
      [ ("location", "https://nqsb.io") ]

  let h_notice tcp =
    let tags = add_tag peer_tag (TCP.dst tcp) in
    TCP.write tcp moved_permanently >>= fun r ->
    (match r with
     | Error e -> Logs.warn (fun m -> m ~tags "TCP error %a" TCP.pp_write_error e)
     | Ok () -> Logs.info (fun m -> m ~tags "HTTP responded")) ;
    TCP.close tcp

  let dispatch nqsb usenix tron tls =
    match TLS.epoch tls with
    | Error () ->
      Logs.warn (fun m -> m ?tags:(Lwt.get stored_tags)
                    "error while retrieving epoch, serving nqsb.io") ;
      nqsb
    | Ok e ->
      let tags = add_tag tls_tag e in
      match e.Tls.Core.own_name with
      | Some "usenix15.nqsb.io" ->
        Logs.info (fun m -> m ~tags "serving usenix pdf") ;
        usenix
      | Some "tron.nqsb.io" ->
        Logs.info (fun m -> m ~tags "serving tron pdf") ;
        tron
      | Some "nqsb.io" ->
        Logs.info (fun m -> m ~tags "serving nqsb.io") ;
        nqsb
      | Some x ->
        Logs.info (fun m -> m ~tags "unknown SNI (%s), serving nqsb.io" x) ;
        nqsb
      | None ->
        Logs.info (fun m -> m ~tags "no sni, serving nqsb.io") ;
        nqsb

  let start stack keys kv _ _ =
    let d_nqsb = [ header "text/html;charset=utf-8" ; Page.render ] in
    read_pdf kv "nqsbtls-usenix-security15.pdf" >>= fun d_usenix ->
    read_pdf kv "tron.pdf" >>= fun d_tron ->
    let f = dispatch d_nqsb d_usenix d_tron in

    read_cert keys "nqsb" >>= fun c_nqsb ->
    read_cert keys "usenix15" >>= fun c_usenix ->
    read_cert keys "tron" >>= fun c_tron ->
    let config = Tls.Config.server ~certificates:(`Multiple_default (c_nqsb, [ c_usenix ; c_tron])) () in

    S.listen_tcpv4 stack ~port:80 h_notice ;
    S.listen_tcpv4 stack ~port:443 (tls_accept ~f config) ;
    S.listen stack
end
