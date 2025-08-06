(use-modules (json))

(define %singbox-streamer
  (let ((direct-process
          '())
        (direct-rules
          '())
        (proxy "..."))
    `(("log"
        ("level" . "warn")
        ("output" . "singbox.log"))
      ("dns"
        ("servers"
          . #((("type" . "tls")
              ("tag" . "DNS Proxy")
              ("detour" . "DNS OUT")
              ("server" . "1.1.1.1"))
              (("type" . "local")
               ("tag" . "DNS Direct")))))
      ("ntp")
      ("certificate")
      ("endpoints")
      ("inbounds")
      ("outbounds")
      ("route")
      ("services")
      ("experimental"))))

(call-with-output-file "config.json"
  (lambda (port)
    (display (scm->json-string %singbox-streamer #:pretty #t) port)))
