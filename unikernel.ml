open Lwt.Infix
open Mirage_types_lwt

module Main (R : RANDOM) (P : PCLOCK) (C : MCLOCK) (T : TIME) (S : STACKV4) (KEYS : KV_RO) (KV : KV_RO) = struct
  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)
  module D = Dns_mirage_certify.Make(R)(P)(T)(S)
  module X = Tls_mirage.X509(KEYS)(P)
  module M = Monitoring_experiments.M.S(T)(P)(C)(S)

  type s = {
    mutable http : int ;
    mutable nqsb : int ;
    mutable usenix : int ;
    mutable tron : int ;
    mutable default : int ;
    mutable none : int ;
  }
  let stat = { http = 0 ; nqsb = 0 ; usenix = 0 ; tron = 0 ; default = 0 ; none = 0 }

  let metrics =
    let open Metrics in
    let doc = "nqsbio statistics" in
    let data stat =
      Data.v
        [ int "http" stat.http
        ; int "nqsb" stat.nqsb
        ; int "usenix" stat.usenix
        ; int "tron" stat.tron
        ; int "none" stat.none
        ; int "default" stat.default ]
    in
    Src.v ~doc ~tags:Metrics.Tags.[] ~data "nqsbio"

  let m () =
    Metrics.add metrics (fun x -> x) (fun d -> d stat)

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
    stat.http <- succ stat.http; m ();
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
        stat.usenix <- succ stat.usenix; m ();
        usenix
      | Some "tron.nqsb.io" ->
        Logs.info (fun m -> m ~tags "serving tron pdf") ;
        stat.tron <- succ stat.tron; m ();
        tron
      | Some "nqsb.io" ->
        Logs.info (fun m -> m ~tags "serving nqsb.io") ;
        stat.nqsb <- succ stat.nqsb; m ();
        nqsb
      | Some x ->
        Logs.info (fun m -> m ~tags "unknown SNI (%s), serving nqsb.io" x) ;
        stat.default <- succ stat.default; m ();
        nqsb
      | None ->
        Logs.info (fun m -> m ~tags "no sni, serving nqsb.io") ;
        stat.none <- succ stat.none; m ();
        nqsb

  let start _ _mclock _pclock _time stack keys kv _ _ info =
    Logs.info (fun m -> m "used packages: %a"
                  Fmt.(Dump.list @@ pair ~sep:(unit ".") string string)
                  info.Mirage_info.packages) ;
    Logs.info (fun m -> m "used libraries: %a"
                  Fmt.(Dump.list string) info.Mirage_info.libraries) ;
    X.certificate keys (`Name "nqsbio") >>= fun (certs, key) ->
    let c = `Single (certs, key) in
    M.create stack ~hostname:"nqsb.nqsbio" c;
    let hostname = Domain_name.of_string_exn "nqsb.io"
    and additional_hostnames =
      List.map Domain_name.of_string_exn[ "tron.nqsb.io" ; "usenix15.nqsb.io" ]
    in
    D.retrieve_certificate ~ca:`Production
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
