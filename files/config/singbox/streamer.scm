(use-modules (json))

(define (sing-box-streamer file-name)
  
  (define %config
    `(("log"
       ("disabled" . #f)
       ("level" . "warn")
       ("timestamp" . #t))
      ("dns"
       ("servers"
	. #((("type" . "local")
             ("tag" . "direct_dns"))))
       ("rules"
	. #((("server" . "direct_dns")
	     ("strategy" . "ipv4_only")))))
      ("inbounds"
       . #((("type" . "direct")
	    ("tag" . "dns_in")
	    ("listen" . "0.0.0.0")
            ("listen_port" . 53)
            ("network" . "udp"))
	   (("type" . "vless")
            ("tag" . "vless_in")
            ("listen" . "127.0.0.1")
            ("listen_port" . 7890)
            ("users"
             . #((("name" . "chiko")
                  ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid)))))
	    ("transport"
	     ("type" . "ws")
	     ("path" . ,(nyapasu-ref 'ws-transport-path))))))
      ("outbounds"
       . #((("type" . "direct")
	    ("tag" . "direct_out")
	    ("domain_resolver"
	     ("server" . "direct_dns")
	     ("strategy" . "ipv4_only")))))
      ("route"
       ("final" . "direct_out")
       ("default_domain_resolver" . "direct_dns"))))
  
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %config #:pretty #t) port))))
