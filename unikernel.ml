open Lwt.Infix
open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (T : TIME) (S : STACKV4) (KV : KV_RO) = struct
  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)
  module D = Dns_mirage_certify.Make(R)(P)(T)(S)

  let http_header ~status xs =
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

  let header content_type len =
    http_header
      ~status:"HTTP/1.1 200 OK"
      [ ("Content-Type", content_type) ;
        ("Content-length", string_of_int len) ;
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
    [ header "application/pdf" (Cstruct.len data) ; data ]

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
      Fmt.(pair ~sep:(unit ":") Ipaddr.V4.pp int)

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

  let start _ pclock _time stack kv _ _ info =
    Logs.info (fun m -> m "used packages: %a"
                  Fmt.(Dump.list @@ pair ~sep:(unit ".") string string)
                  info.Mirage_info.packages) ;
    Logs.info (fun m -> m "used libraries: %a"
                  Fmt.(Dump.list string) info.Mirage_info.libraries) ;

    let hostname = Domain_name.of_string_exn "nqsb.io"
    and additional_hostnames =
      List.map Domain_name.of_string_exn[ "tron.nqsb.io" ; "usenix15.nqsb.io" ]
    in
    D.retrieve_certificate stack pclock ~dns_key:(Key_gen.dns_key ())
      ~hostname ~additional_hostnames ~key_seed:(Key_gen.key_seed ())
      (Key_gen.dns_server ()) (Key_gen.dns_port ()) >>= fun certificates ->
    let config = Tls.Config.server ~certificates () in

    let d_nqsb =
      let page = Page.render in
      [ header "text/html;charset=utf-8" (Cstruct.len page) ; page ]
    in
    read_pdf kv "nqsbtls-usenix-security15.pdf" >>= fun d_usenix ->
    read_pdf kv "tron.pdf" >>= fun d_tron ->
    let f = dispatch d_nqsb d_usenix d_tron in

    S.listen_tcpv4 stack ~port:80 h_notice ;
    S.listen_tcpv4 stack ~port:443 (tls_accept ~f config) ;
    S.listen stack
end
