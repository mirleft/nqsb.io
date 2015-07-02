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
  in
  let a_pinata    = link ~href:"http://ownme.ipredator.se" <:html<BTC Piñata>>
  and a_tls       = link ~href:(mirleft ^ "ocaml-tls") <:html<TLS>>
  and a_x509      = link ~href:(mirleft ^ "ocaml-x509") <:html<X.509>>
  and a_nocrypto  = link ~href:(mirleft ^ "ocaml-nocrypto") <:html<nocrypto>>
  and a_asn1      = link ~href:(mirleft ^ "ocaml-asn1-combinators") <:html<ASN.1>>
  and a_tls_doc   = link ~href:(doc ^ "tls") <:html<API documentation>>
  and a_x509_doc  = link ~href:(doc ^ "x509") <:html<API documentation>>
  and a_tracechk  = link ~href:(hannesm ^ "trace-checker") <:html<pcap trace checker>>
  and a_tlstunnel = link ~href:(hannesm ^ "tlstunnel") <:html<tlstunnel>>
  and a_tlsclient = link ~href:(hannesm ^ "tlsclient") <:html<tlsclient>>
  and a_jackline  = link ~href:(hannesm ^ "jackline") <:html<jackline>>
  and a_certify   = link ~href:(github ^ "yomimono/ocaml-certify") <:html<certify>>
  and a_otr       = link ~href:(hannesm ^ "ocaml-otr") <:html<OTR>>
  and a_handshake = link ~href:"https://tls.openmirage.org" <:html<TLS handshake visualisation>>
  and a_ipredator = link ~href:"https://www.ipredator.se" <:html<IPredator>>
  and a_wp_tls    = link ~href:(wp ^ "Transport_Layer_Security") <:html<WP: TLS>>
  and a_paper     = link ~href:"/nqsbtls-usenix-security15.pdf" <:html<Usenix Security 2015 paper>>
  and a_mirage    = link ~href:"https://mirage.io" <:html<MirageOS>>
  in
  <:html<

    <div id="content">

      <h2>Not-quite-so-broken</h2>

      <br/>

      <p>Protocol implementations have lots of security flaws.  The immediate causes of these are often programming errors, e.g. in memory management, but the root causes are more fundamental: the challenges of interpreting the ambiguous prose specification (RFCs), the complexities inherent in large APIs and code bases, inherently unsafe programming choices, and the impossibility of directly testing conformance between implementations and the specification.</p>

      <p><i>Not-quite-so-broken</i> is the theme of our re-engineered approach to security protocol specification and implementation that addresses these root causes.  The same code serves two roles: it is both a specification, executable as a test oracle to check conformance of traces from arbitrary implementations, and a usable implementation; a modular and declarative programming style provides clean separation between its components.  Many security flaws are thus excluded by construction.</p>

      <p>Read more in our $a_paper$.  We implemented some libraries in OCaml using this approach:</p>

      <ul>
        <li><p>$a_tls$ ($a_tls_doc$), the most widely used security protocol on the Internet ($a_wp_tls$)</p></li>
        <li><p>$a_x509$ ($a_x509_doc$) certificate handling (including public and private RSA keys (PKCS8), certificate signing requests (PKCS10))</p></li>
        <li><p>$a_asn1$ parser and unparser combinators</p></li>
        <li><p>$a_nocrypto$ underlying cryptographic primitives (symmetric: 3DES, AES; hash: MD5, SHA 1, SHA 2; asymmetric: DH, DSA, RSA; CSPRNG: Fortuna; ECB/CBC/CCM/GCM modes)</p></li>
        <br/>
        <li><p>$a_otr$, the Off-the-record protocol</p></li>
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

      <p>Thanks to $a_ipredator$ for hosting.</p>
    </div>

  >>

let render =
  Cstruct.of_string @@ Html.to_string @@
    wrap_body ~title:"nqsb" ~body:content
