(use-modules (json))

(define (sing-box-listener)
  (define %rule-sets
    (let ((geosite
           (lambda (rule-set)
             `(("type" . "remote")
               ("tag" . ,(format #f "geosite-~a" rule-set))
               ("url" . ,(format #f "https://raw.githubusercontent.com/MetaCubeX/meta-rules-dat/refs/heads/sing/geo/geosite/~a.srs" rule-set))
               ("download_detour" . "vless_out"))))
          (geoip
           (lambda (rule-set)
             `(("type" . "remote")
               ("tag" . ,(format #f "geoip-~a" rule-set))
               ("url" . ,(format #f "https://raw.githubusercontent.com/MetaCubeX/meta-rules-dat/refs/heads/sing/geo/geoip/~a.srs" rule-set))
               ("download_detour" . "vless_out")))))
      (append
       (map geosite
            '("category-ads-all"
              "category-dev"
              "cn"
              "gfw"
              "private"
              "stripe"))
       (map geoip
            '("cn"
              "telegram")))))

  (define %block-rules
    '((("rule_set" . "geosite-category-ads-all"))))

  (define %direct-rules
    '((("protocol" . "bittorrent"))
      (("rule_set" . "geosite-private"))
      (("rule_set" . "geosite-cn"))
      (("rule_set" . "geoip-cn"))))

  (define %proxy-rules
    '((("rule_set" . "geosite-gfw"))
      (("rule_set" . "geosite-category-dev"))
      (("rule_set" . "geosite-stripe"))
      (("domain_suffix" . "boiledscript.com"))
      (("domain_suffix" . "freedesktop.org"))
      (("rule_set" . "geoip-telegram"))
      (("inbound" . "vless_out"))))
  
  (define %config
      `(("log"
	 ("level" . "warn"))
	("dns"
	 ("servers"
          . #((("type" . "https")
               ("tag" . "cloudflare")
               ("server" . "1.1.1.1")
	       ("detour" . "vless_out"))
	      (("type" . "udp")
	       ("tag" . "tailscale_dns")
	       ("server" . "100.100.100.100"))
              (("type" . "local")
               ("tag" . "dns_direct"))))
	 ("rules"
	  . #((("domain_suffix" . #(".tailb8a678.ts.net"))
	       ("domain" . #("chikocloud" "chikopara" "dreamtwi" "yumemios"))
	       ("server" . "tailscale_dns")))))
	("inbounds"
	 . #((("type". "mixed")
              ("tag" . "mixed-in")
              ("listen" . "::")
              ("listen_port" . 7890))
             (("type" . "tproxy")
              ("listen" . "::")
              ("listen_port" . 7891)
              ("tag" . "tproxy-in"))))
	("outbounds"
	 . #((("type" . "vless")
              ("tag" . "vless_out")
	      ("domain_resolver" . "tailscale_dns")
              ("server" . "chikocloud.tailb8a678.ts.net")
              ("server_port" . 7890)
              ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
              ("tls"
               ("enabled" . #f)))
             (("type" . "direct")
              ("tag" . "out_direct"))
             (("type" . "block")
              ("tag" . "block"))))
	("route"
	 ("rules"
          . #((("action" . "sniff"))
              (("protocol" . "dns")
               ("action" . "hijack-dns"))
              (("ip_is_private" . #t)
               ("outbound" . "out_direct"))
	      (("outbound" . "direct_out")
	       ("action" . "resolve")
	       ("strategy" . "ipv4_only"))
              ,@(map (lambda (rule)
                       `(,@rule
			 ("outbound" . "out_direct")))
                     %direct-rules)
              (("process_name" . #(,@%direct-process))
               ("outbound" . "out_direct"))))
	 ("rule_set"
          . #((("type" . "remote")
               ("tag" . "geosite-location-cn")
               ("url" . "https://raw.githubusercontent.com/SagerNet/sing-geosite/refs/heads/rule-set/geosite-geolocation-cn.srs")
               ("download_detour" . "vless_out"))
                  (("type" . "remote")
	       ("tag" . "geosite-location-!cn")
	       ("url" . "https://raw.githubusercontent.com/SagerNet/sing-geosite/refs/heads/rule-set/geosite-geolocation-!cn.srs")
	       ("download_detour" . "vless_out"))
               (("type" . "remote")
		("tag" . "geoip-cn")
		("url" . "https://raw.githubusercontent.com/SagerNet/sing-geoip/refs/heads/rule-set/geoip-cn.srs")
		("download_detour" . "vless_out"))))
	 ("final" . "vless_out")
	 ("default_domain_resolver" . "cloudflare"))))
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %config #:pretty #t) port))))
