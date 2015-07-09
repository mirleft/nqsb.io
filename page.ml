open Cow

let wrap_body ~title ~body =
  <:html<
    <html>
      <head>
        <title>$Html.html_of_string title$</title>
        <style>
          body {
            /* font-family: sans-serif; */
            font-family: monospace;
            color: #333;
          }
          #content {
            margin: 10% 0 10% 15%;
            width: 45%;
            /* line-height: 12px; */
          }
          #content h3 {
            font-size: 35px;
          }
          a, a:visited {
            color: #333;
            text-decoration: none;
            font-weight: bold;
          }
          #logo {
            z-index: -1;
            opacity: 0.9;
            position: fixed;
            width: 40%;
            top: 0;
            right: 0;
          }
        </style>
      </head>
      <body>$body$</body>
    </html>
  >>

let link ~href child = [ Html.a ~href:(Uri.of_string href) child ]

let content =
  let github = "https://github.com/" in
  let mirleft = github ^ "mirleft/"
  and hannesm = github ^ "hannesm/"
  and wp = "https://en.wikipedia.org/wiki/"
  and doc = "https://mirleft.github.io/ocaml-"
  and blog = "https://mirage.io/blog/"
  in
  let a_pinata    = link ~href:"http://ownme.ipredator.se" <:html<BTC Piñata>>
  and a_tls       = link ~href:(mirleft ^ "ocaml-tls") <:html<TLS>>
  and a_x509      = link ~href:(mirleft ^ "ocaml-x509") <:html<X.509>>
  and a_nocrypto  = link ~href:(mirleft ^ "ocaml-nocrypto") <:html<nocrypto>>
  and a_asn1      = link ~href:(mirleft ^ "ocaml-asn1-combinators") <:html<ASN.1>>
  and a_tls_doc   = link ~href:(doc ^ "tls") <:html<API documentation>>
  and a_x509_doc  = link ~href:(doc ^ "x509") <:html<API documentation>>
  and a_nc_doc    = link ~href:(doc ^ "nocrypto") <:html<API documentation>>
  and a_tracechk  = link ~href:(hannesm ^ "trace-checker") <:html<pcap trace checker>>
  and a_tlstunnel = link ~href:(hannesm ^ "tlstunnel") <:html<tlstunnel>>
  and a_tlsclient = link ~href:(hannesm ^ "tlsclient") <:html<tlsclient>>
  and a_jackline  = link ~href:(hannesm ^ "jackline") <:html<jackline>>
  and a_certify   = link ~href:(github ^ "yomimono/ocaml-certify") <:html<certify>>
  and a_otr       = link ~href:(hannesm ^ "ocaml-otr") <:html<OTR>>
  and a_otr_doc   = link ~href:"https://hannesm.github.io/ocaml-otr" <:html<API documentation>>
  and a_handshake = link ~href:"https://tls.openmirage.org" <:html<TLS handshake visualisation>>
  and a_ipredator = link ~href:"https://www.ipredator.se" <:html<IPredator>>
  and a_wp_tls    = link ~href:(wp ^ "Transport_Layer_Security") <:html<WP: TLS>>
  and a_paper     = link ~href:"/nqsbtls-usenix-security15.pdf" <:html<Not-quite-so-broken TLS: lessons in re-engineering a security protocol specification and implementation>>
  and a_mirage    = link ~href:"https://mirage.io" <:html<MirageOS>>

  and a_blog_intro    = link ~href:(blog ^ "introducing-ocaml-tls") <:html<Introducing transport layer security (TLS) in pure OCaml>>
  and a_blog_nc       = link ~href:(blog ^ "introducing-nocrypto") <:html<Building the nocrypto library core>>
  and a_blog_x509     = link ~href:(blog ^ "introducing-x509") <:html<Adventures in X.509 certificate parsing and validation>>
  and a_blog_asn1     = link ~href:(blog ^ "introducing-asn1") <:html<ASN.1 and notation embedding>>
  and a_blog_attacks  = link ~href:(blog ^ "ocaml-tls-api-internals-attacks-mitigation") <:html<Protocol implementation and mitigations to known attacks>>
  and a_blog_pinata   = link ~href:(blog ^ "announcing-bitcoin-pinata") <:html<Smash the Bitcoin Pinata for fun and profit!>>
  and a_blog_why      = link ~href:(blog ^ "why-ocaml-tls") <:html<Why OCaml-TLS?>>
  and a_blog_mirage25 = link ~href:(blog ^ "announcing-mirage-25-release") <:html<MirageOS v2.5 with full TLS support>>
  and a_blog_pinata_results = link ~href:(blog ^ "bitcoin-pinata-results") <:html<Reviewing the Bitcoin Pinata>>
  and a_blog_seal = link ~href:(blog ^ "mirage-seal") <:html<Easy HTTPS Unikernels with mirage-seal>>
  and a_31c3 = link ~href:"http://media.ccc.de/browse/congress/2014/31c3_-_6443_-_en_-_saal_2_-_201412271245_-_trustworthy_secure_modular_operating_system_engineering_-_hannes_-_david_kaloper.html#video" <:html<31c3 talk: Trustworthy secure modular operating system engineering>>
  and a_ohm8 = link ~href:"http://monoxyd.de/20150408-ohm-008-ohne-heftige-mangel" <:html<(german) OHM #008 – Ohne Heftige Mängel: TLS und dessen Probleme; robuste Implementierung von Sicherheitsprotokollen; OCaml-TLS und MirageOS>>
  and a_amir_pinata = link ~href:"http://amirchaudhry.com/bitcoin-pinata/" <:html<Amir's post>>
  and a_mirage_pinata = link ~href:(blog ^ "announcing-bitcoin-pinata") <:html<MirageOS post>>
  and a_golem_pinata = link ~href:"http://www.golem.de/news/bug-bounty-hacker-sollen-mirageos-auf-schwachstellen-pruefen-1502-112289.html" <:html<(german) Golem post>>
  in
  <:html<

    <div id="content">

      <h2>Not-quite-so-broken</h2>

      <br/>

      <p>Protocol implementations have lots of security flaws.  The immediate causes of these are often programming errors, e.g. in memory management, but the root causes are more fundamental: the challenges of interpreting the ambiguous prose specification (RFCs), the complexities inherent in large APIs and code bases, inherently unsafe programming choices, and the impossibility of directly testing conformance between implementations and the specification.</p>

      <p><i>Not-quite-so-broken</i> is the theme of our re-engineered approach to security protocol specification and implementation that addresses these root causes.  The same code serves two roles: it is both a specification, executable as a test oracle to check conformance of traces from arbitrary implementations, and a usable implementation; a modular and declarative programming style provides clean separation between its components.  Many security flaws are thus excluded by construction.</p>

      <p>Read more in our paper: $a_paper$ by David Kaloper-Meršinjak, Hannes
Mehnert, Anil Madhavapeddy and Peter Sewell, published at Usenix Security 2015.  We implemented some libraries in OCaml using this approach:</p>

      <ul>
        <li><p>$a_tls$ ($a_tls_doc$), the most widely used security protocol on the Internet ($a_wp_tls$)</p></li>
        <li><p>$a_x509$ ($a_x509_doc$) certificate handling (including public and private RSA keys (PKCS8), certificate signing requests (PKCS10))</p></li>
        <li><p>$a_asn1$ parser and unparser combinators</p></li>
        <li><p>$a_nocrypto$ ($a_nc_doc$) underlying cryptographic primitives (symmetric: 3DES, AES; hash: MD5, SHA 1, SHA 2; asymmetric: DH, DSA, RSA; CSPRNG: Fortuna; ECB/CBC/CCM/GCM modes)</p></li>
        <li><p>$a_otr$ ($a_otr_doc$), the Off-the-record protocol</p></li>
      </ul>

      <br/>

      <p>Some artifacts which use our libraries:</p>

      <ul>
        <li><p>$a_mirage$, a library operating system</p></li>
        <li><p>$a_pinata$, our bitcoin bait</p></li>
        <li><p>$a_handshake$, our interactive visualisation</p></li>
        <li><p>$a_tlstunnel$, an application handling TLS, forwarding the plaintext to another service via TCP</p></li>
        <li><p>$a_tlsclient$, a TLS client</p></li>
        <li><p>$a_certify$, which handles certificates, certificate signing requests, and basic CA functionality</p></li>
        <li><p>$a_tracechk$, which lets you validate recorded TLS sessions</p></li>
        <li><p>$a_jackline$, a terminal XMPP client using OTR and TLS</p></li>
      </ul>

      <br/>

      <p>Media about nqsb at mirage.io:</p>

      <ul>
        <li><p>7 Jul 2015: $a_blog_seal$</p></li>
        <li><p>29 Jun 2015: $a_blog_pinata_results$</p></li>
        <li><p>26 Jun 2015: $a_blog_mirage25$</p></li>
        <li><p>26 Jun 2015: $a_blog_why$</p></li>
        <li><p>4 Apr 2015: $a_ohm8$</p></li>
        <li><p>10 Feb 2015: $a_blog_pinata$ ($a_amir_pinata$, $a_mirage_pinata$, $a_golem_pinata$)</p></li>
        <li><p>27 December 2014: $a_31c3$</p></li>
        <li><p>14 Jul 2014: $a_blog_attacks$</p></li>
        <li><p>11 Jul 2014: $a_blog_asn1$</p></li>
        <li><p>10 Jul 2014: $a_blog_x509$</p></li>
        <li><p>9 Jul 2014: $a_blog_nc$</p></li>
        <li><p>8 Jul 2014: $a_blog_intro$</p></li>
      </ul>

      <p>Thanks to $a_ipredator$ for hosting.</p>
    </div>

  >>

let render =
  Cstruct.of_string @@ Html.to_string @@
    wrap_body ~title:"nqsb" ~body:content
