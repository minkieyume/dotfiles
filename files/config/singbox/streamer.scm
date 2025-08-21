(use-modules (json))

(define %singbox-streamer
  (let ((direct-process
          '())
         (direct-rules
           '()))
    `(("log"
        ("level" . "warn"))
       ("dns"
         ("servers"
           . #((("type" . "tls")
                ("tag" . "cloudflare")
                ("server" . "1.1.1.1"))
               (("type" . "local")
                ("tag" . "direct")))))
       ("inbounds"
         . #((("type" . "vless")
               ("tag" . "vitural less")
               ("listen" . "::")
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

(define (output-singbox-streamer file-name)
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %singbox-streamer #:pretty #t) port))))
