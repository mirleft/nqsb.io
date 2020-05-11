open Lwt.Infix

module Main (C : Mirage_console.S) (R : Mirage_random.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (P : Mirage_clock.PCLOCK) (S : Mirage_stack.V4) (KV : Mirage_kv.RO) (Management : Mirage_stack.V4) = struct
  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)

  let http_resource =
    Monitoring_experiments.counter_metrics ~f:(fun x -> x) "nqsbio"

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

  let read_pdf kv name =
    KV.get kv (Mirage_kv.Key.v name) >>= function
    | Error e ->
      Logs.warn (fun m -> m "kv: error while calling get %s: %a" name KV.pp_error e);
      Lwt.fail (invalid_arg "failed")
    | Ok data ->
      let cs = Cstruct.of_string data in
      Lwt.return [ header "application/pdf" (Cstruct.len cs) ; cs ]

  let tls_accept ~f cfg tcp =
    TLS.server_of_flow cfg tcp >>= function
    | Error e ->
      Logs.warn (fun m -> m "TLS error %a" TLS.pp_write_error e);
      TCP.close tcp
    | Ok tls ->
      let data = f tls in
      (TLS.writev tls data >|= function
        | Ok () -> ()
        | Error we ->
          Logs.warn (fun m -> m "TLS write error %a" TLS.pp_write_error we)) >>= fun () ->
      TLS.close tls

  let moved_permanently =
    http_header ~status:"HTTP/1.1 301 Moved permanently"
      [ ("location", "https://nqsb.io") ]

  let h_notice tcp =
    Metrics.add http_resource (fun x -> x) (fun d -> d "http");
    TCP.write tcp moved_permanently >>= fun r ->
    (match r with
     | Error e -> Logs.warn (fun m -> m  "TCP error %a" TCP.pp_write_error e)
     | Ok () -> ());
    TCP.close tcp

  let dispatch nqsb usenix tron tls =
    match TLS.epoch tls with
    | Error () ->
      Logs.warn (fun m -> m "error while retrieving epoch, serving nqsb.io") ;
      nqsb
    | Ok e ->
      let name, data =
        match e.Tls.Core.own_name with
        | Some "usenix15.nqsb.io" -> "usenix", usenix
        | Some "tron.nqsb.io" -> "tron", tron
        | Some "nqsb.io" -> "nqsb", nqsb
        | Some x -> x, nqsb
        | None -> "none", nqsb
      in
      Metrics.add http_resource (fun x -> x) (fun d -> d name);
      data

  module D = Dns_certify_mirage.Make(R)(P)(T)(S)
  module Monitoring = Monitoring_experiments.Make(T)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(P)(Management)

  let start c _random _time _mclock _pclock stack kv management =
    let hostname = Key_gen.name ()
    and syslog = Key_gen.syslog ()
    and monitor = Key_gen.monitor ()
    in
    if Ipaddr.V4.compare syslog Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
    else
      Logs.set_reporter (Syslog.create c management syslog ~hostname ());
    if Ipaddr.V4.compare monitor Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
    else
      Monitoring.create ~hostname monitor management;
    let hostname = Domain_name.(of_string_exn "nqsb.io" |> host_exn)
    and additional_hostnames =
      List.map (fun s -> Domain_name.(of_string_exn s |> host_exn))
        [ "tron.nqsb.io" ; "usenix15.nqsb.io" ]
    in
    D.retrieve_certificate
      stack ~dns_key:(Key_gen.dns_key ())
      ~hostname ~additional_hostnames ~key_seed:(Key_gen.key_seed ())
      (Key_gen.dns_server ()) (Key_gen.dns_port ()) >>= function
    | Error (`Msg m) -> Lwt.fail_with m
    | Ok certificates ->
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
