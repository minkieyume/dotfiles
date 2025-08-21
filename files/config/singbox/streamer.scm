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
               ("listen" . "0.0.0.0")
               ("listen_port" . 7890)
               ("users"
                 . #((("name" . "chiko")
                     ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
                     ("flow" . "xtls-rprx-vision"))))
               ("tls"
                 ("enabled" . #f)))))
       ("route"
         ("default_domain_resolver" . "direct")))))

(define (output-singbox-streamer file-name)
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %singbox-streamer #:pretty #t) port))))
