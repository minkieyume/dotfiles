(use-modules (json))

(define (sing-box-streamer file-name)
  
  (define %config
    `(("log"
       ("level" . "warn"))
      ("dns"
       ("servers"
	. #((("type" . "local")
             ("tag" . "direct"))))
       ("rules"
	. #((("server" . "direct")
	     ("strategy" . "ipv4_only")))))
      ("inbounds"
       . #((("type" . "direct")
	    ("tag" . "dns_in")
	    ("listen" . "0.0.0.0")
            ("listen_port" . 53)
            ("network" . "udp"))
	   (("type" . "vless")
            ("tag" . "proxy_in")
            ("listen" . "0.0.0.0")
            ("listen_port" . 7890)
            ("users"
             . #((("name" . "chiko")
                  ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid)))))
            ("tls"
             ("enabled" . #f)))))
      ("outbounds"
       . #(("type" . "direct")
	   ("tag" . "direct_out")))
      ("route"
       ("rules"
	. #((("action" . "sniff"))
            (("protocol" . "dns")
             ("action" . "hijack-dns"))
	    (("action" . "resolve")
	     ("strategy" . "ipv4_only"))))
       ("final" . "direct_out")
       ("default_domain_resolver" . "direct"))))
  
  
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %config #:pretty #t) port))))
