(use-modules (json))
(load "../../../reconfigure/nyapasu.scm")

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
                ("server" . "1.1.1.1"))
               (("type" . "local")
                ("tag" . "DNS Direct")))))
       ("inbounds"
         . #((("type" . "vless")
               ("tag" . "vitural less")
               ("listen" . "0.0.0.0")
               ("listen_port" . 7890)
               ("users"
                 . #((("name" . "chiko")
                     ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
                     ("flow" . "xtls-rprx-vision"))))
               ("tls"
                 ("enabled" . #t)
                 ("reality"
                   ("enabled" . #t)
                   ("handshake"
                     ("server" . "www.bilibili.com")
                     ("server_port" . 443))
                   ("private_key" . ,(nyapasu-ref 'reality-private-key))
                   ("short_id"
                     . #("c10a110e")))))))
       ("route"
         ("default_domain_resolver" . "direct")))))

(call-with-output-file "../../../reconfigure/streamer.json"
  (lambda (port)
    (display (scm->json-string %singbox-streamer #:pretty #t) port)))
