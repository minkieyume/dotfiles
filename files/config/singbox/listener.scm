(use-modules (json))

(define (sing-box-listener file-name)
  (define %rule-sets
    (let ((geosite
           (lambda (rule-set)
             `(("type" . "remote")
               ("tag" . ,(format #f "geosite-~a" rule-set))
               ("url" . ,(format #f "https://raw.githubusercontent.com/MetaCubeX/meta-rules-dat/refs/heads/sing/geo/geosite/~a.srs" rule-set))
               ("download_detour" . "out_proxy"))))
          (geoip
           (lambda (rule-set)
             `(("type" . "remote")
               ("tag" . ,(format #f "geoip-~a" rule-set))
               ("url" . ,(format #f "https://raw.githubusercontent.com/MetaCubeX/meta-rules-dat/refs/heads/sing/geo/geoip/~a.srs" rule-set))
               ("download_detour" . "out_proxy")))))
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
	      "private"
              "telegram")))))
  (define %direct-process
    '("rclone"
      "steam"
      "sing-box"
      "wineserver"
      "syncthing"
      "yggdrasil"
      "smartdns"
      "PHOGS.exe"))
  
  (define %block-rules
    '((("rule_set" . "geosite-category-ads-all"))))

  (define %direct-rules
    '((("protocol" . "bittorrent"))
      (("protocol" . "stun"))
      (("ip_version" . 6))
      (("rule_set" . "geosite-private"))
      (("rule_set" . "geosite-cn"))
      (("rule_set" . "geoip-cn"))
      (("rule_set" . "geoip-private"))
      (("port" . #(3478)))
      (("domain_suffix" . "neboer.site"))
      (("domain_suffix" . #("syncthing.net" "discovery-announce-v4.syncthing.net")))
      (("domain_suffix" . #("lan" "local" "localhost")))
      (("domain_suffix" . "yumieko.com"))
      (("domain_suffix" . #("frp-add.com" "frp-pet.com" "frp-fit.com")))))

  (define %proxy-rules
    '((("rule_set" . "geosite-gfw"))
      (("rule_set" . "geosite-category-dev"))
      (("rule_set" . "geosite-stripe"))      
      (("domain_suffix" . "boiledscript.com"))
      (("domain_suffix" . "freedesktop.org"))
      (("rule_set" . "geoip-telegram"))
      (("inbound" . "proxy_in"))))
  
  (define %config
    `(("log"
       ("disabled" . #f)
       ("level" . "warn")
       ("timestamp" . #t))
      ("dns"
       ("servers"
        . #((("type" . "https")
	     ("tag" . "cloudflare-doh")
	     ("server" . "cloudflare-dns.com")
	     ("detour" . "out_proxy"))
	    (("type" . "https")
	     ("tag" . "google-doh")
	     ("server" . "dns.google")
	     ("detour" . "out_proxy"))
	    (("type" . "tls")
	     ("tag" . "cloudflare-tls")
	     ("server" . "1.1.1.1"))
	    (("type" . "tls")
	     ("tag" . "google-tls")
	     ("server" . "8.8.8.8"))
	    (("type" . "udp")
	     ("server" . "100.100.100.100")
             ("tag" . "dns_tailscale"))
	    (("type" . "udp")
	     ("server" . "223.5.5.5")
	     ("tag" . "dns_cn"))))
       ("rules"
	. #((("domain_suffix" . #("ts.net"))
	     ("domain" . #("chikocloud" "chikopara" "dreamtwi" "yumemios"))
	     ("server" . "dns_tailscale"))
	    
	    (("process_name" . #(,@%direct-process))
             ("server" . "dns_cn"))
	    
            ,@(map (lambda (rule)
                     `(,@rule
		       ("server" . "dns_cn")))
                   %direct-rules)
	    
	    ,@(map (lambda (rule)
                     `(,@rule
		       ("server" . "cloudflare-doh")
		       ("strategy" . "ipv4_only")))
                   %proxy-rules))))
      ("inbounds"
       . #((("type". "mixed")
            ("tag" . "proxy_in")
            ("listen" . "::")
            ("listen_port" . 7890))
           (("type" . "tproxy")
            ("listen" . "::")
            ("listen_port" . 7891)
            ("tag" . "tproxy_in"))))
      ("outbounds"
       . #((("type" . "vless")
            ("tag" . "out_proxy")
            ("server" . "littlewing.yumieko.com")
            ("server_port" . 443)
            ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
	    ("tls"
	     ("enabled" . #t))
	    ("transport"
	     ("type" . "ws")
	     ("path" . ,(nyapasu-ref 'ws-transport-path))
	     ("max_early_data" . 512)
	     ("early_data_header_name" . "Sec-WebSocket-Protocol")))
           (("type" . "direct")
            ("tag" . "out_direct"))
           (("type" . "block")
            ("tag" . "out_block"))))
      ("route"
       ("rules"
        . #((("action" . "sniff"))
            (("protocol" . "dns")
             ("action" . "hijack-dns"))
            (("ip_is_private" . #t)
             ("outbound" . "out_direct"))
	    ,@(map (lambda (rule)
                     `(,@rule
                       ("outbound" . "out_block")))
                   %block-rules)
	    
	    (("process_name" . #(,@%direct-process))
             ("outbound" . "out_direct"))
	    
            ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_direct")))
                   %direct-rules)
	    
	    ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_proxy")))
                   %proxy-rules)))
       ("rule_set"
        . #(,@%rule-sets))
       ("final" . "out_direct")
       ("default_domain_resolver" . "dns_cn"))))
  
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %config #:pretty #t) port))))
