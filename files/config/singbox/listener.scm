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
                 ("detour" . "DNS OUT")
                 ("server" . "1.1.1.1"))
                (("type" . "local")
                  ("tag" . "direct")))))
       ("inbounds"
         . #((("type". "mixed")
               ("tag" . "mixed-in")
               ("listen" . "::")
               ("listen_port" . "7890")
               ("set_system_proxy" . #t))))
       ("outbounds"
         . #((("type" . "vless")
              ("tag" . "vless-out")
              ("server" . ,(nyapasu-ref 'sing-box-ip))
              ("server_port" . 7890)
              ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
              ("flow" . "xtls-rprx-vision")
              ("tls"
                ("enabled" . #t)
                ("reality"
                  ("enabled" . #t)
                  ("public_key" . ,(nyapasu-ref 'reality-pubic-key))
                  ("short_id"  . "c10a110e"))))))
       ("route"
         ("default_domain_resolver" . "direct")))))

(call-with-output-file "../../../reconfigure/listener.json"
  (lambda (port)
    (display (scm->json-string %singbox-streamer #:pretty #t) port)))
