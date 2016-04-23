open Html5.M

module StringPrinter = struct
    type out = string
    type m = string

    let empty = ""
    let concat = (^)
    let put a = a
    let make a = a
end

module StringHtml = Html5.Make_printer(StringPrinter)

let header title =
  head
    (Html5.M.title (pcdata title))
    ([meta ~a:[a_charset "UTF-8"] ();
      style [ pcdata
    {___|body {
           font-family: monospace;
           color: #333;
         }
         .content {
           margin: 10% 0 10% 15%;
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
      and a_tls_doc = a ~a:[a_href (doc ^ "tls")] [pcdata "API documentation"]
      in
      [ a_tls ; pcdata " (" ; a_tls_doc ; pcdata ")" ; pcdata ", the most widely used security protocol on the Internet (" ]
    and x509 =
      let a_x509 = a ~a:[a_href (mirleft ^ "ocaml-x509")] [pcdata "X.509"]
      and a_x509_doc = a ~a:[a_href (doc ^ "x509")] [pcdata "API documentation"]
      in
      [ a_x509 ; pcdata " (" ; a_x509_doc ; pcdata ")" ;
        pcdata "certificate handling (including public and private RSA keys (PKCS8), certificate signing requests (PKCS10))" ]
    and asn1 =
      let a_asn1 = a ~a:[a_href (mirleft ^ "ocaml-asn1-combinators")] [pcdata "ASN.1"] in
      [ a_asn1 ; pcdata " parser and unparser combinators" ]
    and nocrypto =
      let a_nocrypto = a ~a:[a_href (mirleft ^ "ocaml-nocrypto")] [pcdata "nocrypto"]
      and a_nc_doc = a ~a:[a_href (doc ^ "nocrypto")] [pcdata "API documentation"]
      in
      [ a_nocrypto ; pcdata " (" ; a_nc_doc ; pcdata ")" ;
        pcdata " underlying cryptographic primitives (symmetric: 3DES, AES; hash: MD5, SHA 1, SHA 2; asymmetric: DH, DSA, RSA; CSPRNG: Fortuna; ECB/CBC/CCM/GCM modes)" ]
    and otr =
      let a_otr = a ~a:[a_href (hannesm ^ "ocaml-otr")] [pcdata "OTR"]
      and a_otr_doc = a ~a:[a_href  "https://hannesm.github.io/ocaml-otr"] [pcdata "API documentation"]
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
      li (hannesm ^ "tlstunnel") "tlstunnel" ", an application handling TLS, forwarding the plaintext to another service via TCP" ;
      li (github ^ "mirage/mirage-seal") "mirage-seal" ", an application which produces a stand-alone unikernel serving a directory via https" ;
      li (hannesm ^ "tlsclient") "tlsclient" ", a TLS client" ;
      li (hannesm ^ "jackline") "jackline" ", a terminal XMPP client using OTR and TLS" ;
      li (github ^ "yomimono/ocaml-certify") "certify" ", an application to generate certificates, certificate signing requests, and basic CA signing" ;
      li (hannesm ^ "trace-checker") "pcap trace checker" ", which lets you validate recorded TLS sessions"
    ]

  and blog_entries =
    let li date link descr =
      li [ pcdata (date ^ ": ") ; a ~a:[a_href link] [pcdata descr] ] ;
    and a_blog_pinata = a ~a:[a_href (blog ^ "announcing-bitcoin-pinata")] [pcdata "Smash the Bitcoin Pinata for fun and profit!"]
    and a_amir_pinata = a ~a:[a_href "http://amirchaudhry.com/bitcoin-pinata/"] [pcdata "Amir's post"]
    and a_mirage_pinata = a ~a:[a_href (blog ^ "announcing-bitcoin-pinata")] [pcdata "MirageOS post"]
    and a_golem_pinata = a ~a:[a_href "http://www.golem.de/news/bug-bounty-hacker-sollen-mirageos-auf-schwachstellen-pruefen-1502-112289.html"] [pcdata "(german) Golem post"]
    in
    [
      li "22 Jul 2015" (blog ^ "mirage-entropy") "Organized chaos: managing randomness" ;
      li "07 Jul 2015" (blog ^ "mirage-seal") "Easy HTTPS Unikernels with mirage-seal" ;
      li "29 Jun 2015" (blog ^ "bitcoin-pinata-results") "Reviewing the Bitcoin Pinata" ;
      li "26 Jun 2015" (blog ^ "announcing-mirage-25-release") "MirageOS v2.5 with full TLS support" ;
      li "26 Jun 2015" (blog ^ "why-ocaml-tls") "Why OCaml-TLS?" ;
      li "04 Apr 2015" "http://monoxyd.de/20150408-ohm-008-ohne-heftige-mangel"
        "(german) OHM #008 – Ohne Heftige Mängel: TLS und dessen Probleme; robuste Implementierung von Sicherheitsprotokollen; OCaml-TLS und MirageOS" ;
      Html5.M.li [ pcdata "10 Feb 2015: " ; a_blog_pinata ; pcdata " (" ; a_amir_pinata ; pcdata ", " ; a_mirage_pinata ; pcdata ", " ; a_golem_pinata ; pcdata ")" ] ;
      li "27 Dec 2014"
        "http://media.ccc.de/browse/congress/2014/31c3_-_6443_-_en_-_saal_2_-_201412271245_-_trustworthy_secure_modular_operating_system_engineering_-_hannes_-_david_kaloper.html#video"
        "31c3 talk: Trustworthy secure modular operating system engineering" ;
      li "14 Jul 2014" (blog ^ "ocaml-tls-api-internals-attacks-mitigation") "Protocol implementation and mitigations to known attacks" ;
      li "11 Jul 2014" (blog ^ "introducing-asn1") "ASN.1 and notation embedding" ;
      li "10 Jul 2014" (blog ^ "introducing-x509") "Adventures in X.509 certificate parsing and validation" ;
      li "09 Jul 2014" (blog ^ "introducing-nocrypto") "Building the nocrypto library core" ;
      li "08 Jul 2014" (blog ^ "introducing-ocaml-tls") "Introducing transport layer security (TLS) in pure OCaml"
    ]
  in

  div ~a:[a_class ["content"]] [
    h2 [pcdata "Not quite so broken"] ;
    br () ;
    p [pcdata
         "Protocol implementations have lots of security flaws.  The immediate causes of these are often programming errors, e.g. in memory management, but the root causes are more fundamental: the challenges of interpreting the ambiguous prose specification (RFCs), the complexities inherent in large APIs and code bases, inherently unsafe programming choices, and the impossibility of directly testing conformance between implementations and the specification." ] ;
    p [i [ pcdata "Not-quite-so-broken" ] ;
       pcdata " is the theme of our re-engineered approach to security protocol specification and implementation that addresses these root causes.  The same code serves two roles: it is both a specification, executable as a test oracle to check conformance of traces from arbitrary implementations, and a usable implementation; a modular and declarative programming style provides clean separation between its components.  Many security flaws are thus excluded by construction."] ;
    p [pcdata "Read more in our paper: " ;
       a ~a:[a_href "/nqsbtls-usenix-security15.pdf"] [pcdata "Not-quite-so-broken TLS: lessons in re-engineering a security protocol specification and implementation"] ;
       pcdata " by David Kaloper-Meršinjak, Hannes Mehnert, Anil Madhavapeddy and Peter Sewell, published at Usenix Security 2015.  We implemented some libraries in OCaml using this approach:" ] ;
    ul libs ;
    br () ;
    p [ pcdata "Some artifacts which use our libraries:" ] ;
    ul artifacts ;
    br () ;
    p [ pcdata "Media about nqsb:" ] ;
    ul blog_entries ;
    p [ pcdata "Thanks to " ; a ~a:[a_href "https://www.ipredator.se"] [pcdata "IPredator"] ; pcdata " for hosting." ]
  ]

let render =
  Cstruct.of_string @@ StringHtml.print @@
  let title = "not quite so broken" in
  html
    (header title)
    (body [ content ])
