open Lwt.Infix

module Main (C : Mirage_console.S) (T : Mirage_time.S) (P : Mirage_clock.PCLOCK) (S : Mirage_stack.V4V6) (KV : Mirage_kv.RO) (Management : Mirage_stack.V4V6) = struct
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

  let reply name data tcp =
    (S.TCP.writev tcp data >|= function
      | Ok () -> ()
      | Error we ->
        Logs.warn (fun m -> m "TCP write error %a" S.TCP.pp_write_error we)) >>= fun () ->
    S.TCP.close tcp >|= fun () ->
    Metrics.add http_resource (fun x -> x) (fun d -> d name);

  module Monitoring = Monitoring_experiments.Make(T)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(P)(Management)

  let start c _time _pclock stack kv management =
    let hostname = Key_gen.name ()
    and syslog = Key_gen.syslog ()
    and monitor = Key_gen.monitor ()
    in
    (match syslog with
     | None -> Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
     | Some ip -> Logs.set_reporter (Syslog.create c management ip ~hostname ()));
    (match monitor with
     | None -> Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
     | Some ip -> Monitoring.create ~hostname ip management);
    let d_nqsb =
      let page = Page.render in
      [ header "text/html;charset=utf-8" (Cstruct.len page) ; page ]
    in
    read_pdf kv "nqsbtls-usenix-security15.pdf" >>= fun d_usenix ->
    read_pdf kv "tron.pdf" >>= fun d_tron ->
    S.listen_tcp stack ~port:3000 (reply "nqsb" d_nqsb);
    S.listen_tcp stack ~port:3001 (reply "usenix" d_usenix);
    S.listen_tcp stack ~port:3002 (reply "tron" d_tron);
    S.listen stack
end
