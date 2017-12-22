open Tyxml.Html

let header t =
  head
    (title (pcdata t))
    ([meta ~a:[a_charset "UTF-8"] ();
      style [ pcdata
    {___|body {
           font-family: monospace;
           color: #333;
         }
         .content {
           margin: 3% 0 10% 15%;
           width: 45%;
         }
         a, a:visited {
           color: #333;
           text-decoration: none;
           font-weight: bold;
         }|___} ]
        ])

let content =
  let github = "https://github.com/" in
  let mirleft = github ^ "mirleft/"
  and hannesm = github ^ "hannesm/"
  and doc = "https://mirleft.github.io/ocaml-"
  and blog = "https://mirage.io/blog/"
  in
  let libs =
    let tls =
      let a_tls = a ~a:[a_href (mirleft ^ "ocaml-tls")] [pcdata "TLS"]
      and a_tls_doc = a ~a:[a_href (doc ^ "tls/doc")] [pcdata "API documentation"]
      in
      [ a_tls ; pcdata " (" ; a_tls_doc ; pcdata ")" ; pcdata ", the most widely used security protocol on the Internet (" ]
    and x509 =
      let a_x509 = a ~a:[a_href (mirleft ^ "ocaml-x509")] [pcdata "X.509"]
      and a_x509_doc = a ~a:[a_href (doc ^ "x509/doc")] [pcdata "API documentation"]
      in
      [ a_x509 ; pcdata " (" ; a_x509_doc ; pcdata ")" ;
        pcdata "certificate handling (including public and private RSA keys (PKCS8), certificate signing requests (PKCS10))" ]
    and asn1 =
      let a_asn1 = a ~a:[a_href (mirleft ^ "ocaml-asn1-combinators")] [pcdata "ASN.1"]
      and a_asn1_doc = a ~a:[a_href (doc ^ "asn1-combinators/doc")] [pcdata "API documentation"]
      in
      [ a_asn1 ; pcdata " (" ; a_asn1_doc ; pcdata ")" ;
        pcdata " parser and unparser combinators" ]
    and nocrypto =
      let a_nocrypto = a ~a:[a_href (mirleft ^ "ocaml-nocrypto")] [pcdata "nocrypto"]
      and a_nc_doc = a ~a:[a_href (doc ^ "nocrypto/doc")] [pcdata "API documentation"]
      in
      [ a_nocrypto ; pcdata " (" ; a_nc_doc ; pcdata ")" ;
        pcdata " underlying cryptographic primitives (symmetric: 3DES, AES; hash: MD5, SHA 1, SHA 2; asymmetric: DH, DSA, RSA; CSPRNG: Fortuna; ECB/CBC/CCM/GCM modes)" ]
    and otr =
      let a_otr = a ~a:[a_href (hannesm ^ "ocaml-otr")] [pcdata "OTR"]
      and a_otr_doc = a ~a:[a_href  "https://hannesm.github.io/ocaml-otr/doc"] [pcdata "API documentation"]
      in
      [ a_otr ; pcdata " (" ; a_otr_doc ; pcdata ")" ; pcdata ", the Off-the-record protocol" ]
    in
    List.map (fun x -> li x) [ tls ; x509 ; asn1 ; nocrypto ; otr ]

  and artifacts =
    let li href hrefbody body =
      li [ a ~a:[a_href href] [pcdata hrefbody] ; pcdata body ]
    in [
      li "https://mirage.io" "MirageOS" ", a library operating system" ;
      li "http://ownme.ipredator.se" "BTC Piñata" ", our bitcoin bait" ;
      li "https://tls.openmirage.org" "TLS handshake visualisation" ", our interactive visualisation" ;
      li (mirleft ^ "libnqsb-tls") "libnqsb-tls" ", bindings to C which implement the libtls interface (drop-in replacement for libtls.so)" ;
      li (hannesm ^ "tlstunnel") "tlstunnel" ", an application handling TLS, forwarding the plaintext to another service via TCP" ;
      li (mirleft ^ "tlstools") "tlstools" ", some TLS utilities" ;
      li (github ^ "yomimono/ocaml-certify") "certify" ", an application to generate certificates, certificate signing requests, and basic CA signing" ;
      li (github ^ "mirage/mirage-seal") "mirage-seal" ", an application which produces a stand-alone unikernel serving a directory via https" ;
      li (hannesm ^ "tlsclient") "tlsclient" ", a TLS client" ;
      li (hannesm ^ "jackline") "jackline" ", a terminal XMPP client using OTR and TLS" ;
      li (hannesm ^ "trace-checker") "pcap trace checker" ", which lets you validate recorded TLS sessions"
    ]

  and blog_entries =
    let li date link descr =
      li [ pcdata (date ^ ": ") ; a ~a:[a_href link] [pcdata descr] ] ;
    and a_blog_pinata = a ~a:[a_href (blog ^ "announcing-bitcoin-pinata")] [pcdata "Smash the Bitcoin Pinata for fun and profit!"]
    and a_amir_pinata = a ~a:[a_href "http://amirchaudhry.com/bitcoin-pinata/"] [pcdata "Amir's post"]
    and a_mirage_pinata = a ~a:[a_href (blog ^ "announcing-bitcoin-pinata")] [pcdata "MirageOS post"]
    and a_golem_pinata = a ~a:[a_href "http://www.golem.de/news/bug-bounty-hacker-sollen-mirageos-auf-schwachstellen-pruefen-1502-112289.html"] [pcdata "(german) Golem post"]
    and emph tag v_link v_data date link descr =
      let venue = a ~a:[a_href v_link] [pcdata ("@" ^ v_data)] in
      li [ b [ pcdata ("(" ^ tag ^ ")") ] ; pcdata (" " ^ date ^ " ") ; venue ; pcdata ": " ; a ~a:[a_href link] [pcdata descr] ] ;
    in
    [
      Tyxml.Html.li [ a ~a:[a_href "https://hannes.nqsb.io"] [pcdata "hannes blog: full stack engineer"] ] ;
      emph "paper" "https://ocaml.org/meetings/ocaml/2016/" "OCaml2016" "2016-09-23" "https://www.cl.cam.ac.uk/%7Ejdy22/papers/ocaml-inside-a-drop-in-replacement-for-libtls.pdf" "OCaml inside: a drop-in replacement for libtls" ;
      emph "paper" "https://www.internetsociety.org/events/ndss-symposium-2016/tron-workshop-programme" "TRON" "2016-02-21" "https://tron.nqsb.io" "Not-quite-so-broken TLS 1.3 Mechanised Conformance Checking" ;
      emph "paper" "https://www.usenix.org/conference/usenixsecurity15" "UsenixSecurity" "2015-08-10" "https://usenix15.nqsb.io" "Not-quite-so-broken TLS: lessons in re-engineering a security protocol specification and implementation" ;
      li "2015-07-22" (blog ^ "mirage-entropy") "Organized chaos: managing randomness" ;
      li "2015-07-07" (blog ^ "mirage-seal") "Easy HTTPS Unikernels with mirage-seal" ;
      li "2015-06-29" (blog ^ "bitcoin-pinata-results") "Reviewing the Bitcoin Pinata" ;
      li "2015-06-26" (blog ^ "announcing-mirage-25-release") "MirageOS v2.5 with full TLS support" ;
      li "2015-06-26" (blog ^ "why-ocaml-tls") "Why OCaml-TLS?" ;
      Tyxml.Html.li [ pcdata "2015-02-10: " ; a_blog_pinata ; pcdata " (" ; a_amir_pinata ; pcdata ", " ; a_mirage_pinata ; pcdata ", " ; a_golem_pinata ; pcdata ")" ] ;
      emph "video" "https://events.ccc.de/congress/2014/wiki/Static:Main_Page" "31c3" "2014-12-27"  "http://media.ccc.de/browse/congress/2014/31c3_-_6443_-_en_-_saal_2_-_201412271245_-_trustworthy_secure_modular_operating_system_engineering_-_hannes_-_david_kaloper.html#video" "Trustworthy secure modular operating system engineering" ;
      li "2014-07-14" (blog ^ "ocaml-tls-api-internals-attacks-mitigation") "Protocol implementation and mitigations to known attacks" ;
      li "2014-07-11" (blog ^ "introducing-asn1") "ASN.1 and notation embedding" ;
      li "2014-07-10" (blog ^ "introducing-x509") "Adventures in X.509 certificate parsing and validation" ;
      li "2014-07-09" (blog ^ "introducing-nocrypto") "Building the nocrypto library core" ;
      li "2014-07-08" (blog ^ "introducing-ocaml-tls") "Introducing transport layer security (TLS) in pure OCaml"
    ]
  in

  div ~a:[a_class ["content"]] [
    h2 [ pcdata "Not quite so broken" ] ;
    br () ;
    p [ pcdata
          "Protocol implementations have lots of security flaws.  The immediate causes of these are often programming errors, e.g. in memory management, but the root causes are more fundamental: the challenges of interpreting the ambiguous prose specification (RFCs), the complexities inherent in large APIs and code bases, inherently unsafe programming choices, and the impossibility of directly testing conformance between implementations and the specification." ] ;
    p [ i [ pcdata "Not-quite-so-broken" ] ;
        pcdata " is the theme of our re-engineered approach to security protocol specification and implementation that addresses these root causes.  The same code serves two roles: it is both a specification, executable as a test oracle to check conformance of traces from arbitrary implementations, and a usable implementation; a modular and declarative programming style provides clean separation between its components.  Many security flaws are thus excluded by construction."] ;
    p [ pcdata "Read more in our paper: " ;
        a ~a:[a_href "https://usenix15.nqsb.io"] [pcdata "Not-quite-so-broken TLS: lessons in re-engineering a security protocol specification and implementation"] ;
        pcdata " by David Kaloper-Meršinjak, Hannes Mehnert, Anil Madhavapeddy and Peter Sewell, published at Usenix Security 2015.  We implemented some libraries in OCaml using this approach:" ] ;
    ul libs ;
    br () ;
    p [ pcdata "Code using nqsb:" ] ;
    ul artifacts ;
    br () ;
    p [ pcdata "Texts about nqsb:" ] ;
    ul blog_entries ;
    p [ pcdata "Parts of this work were supported by " ;
        a ~a:[a_href "http://www.cl.cam.ac.uk/~pes20/rems"] [pcdata "REMS: Rigorous Engineering for Mainstream Systems"] ;
        pcdata " EPSRC Programme Grant EP/K008528/1, and by the European Union’s Seventh Framework Programme FP7/2007–2013 under the User Centric Networking project (no. 611001)." ] ;
    p [ pcdata "Thanks to " ; a ~a:[a_href "https://www.ipredator.se"] [pcdata "IPredator"] ; pcdata " for hosting." ]
  ]

let render =
  let buf = Buffer.create 500 in
  let fmt = Format.formatter_of_buffer buf in
  pp () fmt @@
  html
    (header "not quite so broken")
    (body [ content ]) ;
  Cstruct.of_string @@ Buffer.contents buf
