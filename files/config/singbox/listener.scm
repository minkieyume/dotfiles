(use-modules (json))

(define (sing-box-listener file-name)
  (define %rule-sets
    (let ((geosite
           (lambda (rule-set)
             `(("type" . "remote")
               ("tag" . ,(format #f "geosite-~a" rule-set))
               ("url" . ,(format #f "https://raw.githubusercontent.com/MetaCubeX/meta-rules-dat/refs/heads/sing/geo/geosite/~a.srs" rule-set))
               ("download_detour" . "out_direct"))))
          (geoip
           (lambda (rule-set)
             `(("type" . "remote")
               ("tag" . ,(format #f "geoip-~a" rule-set))
               ("url" . ,(format #f "https://raw.githubusercontent.com/MetaCubeX/meta-rules-dat/refs/heads/sing/geo/geoip/~a.srs" rule-set))
               ("download_detour" . "out_direct")))))
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
      (("rule_set" . "geosite-private"))
      (("rule_set" . "geosite-cn"))
      (("rule_set" . "geoip-cn"))
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
       ("disable" . #f)
       ("level" . "warn")
       ("timestamp" . #t))
      ("dns"
       ("servers"
        . #((("type" . "udp")
	     ("tag" . "dns_proxy")
	     ("server" . "chikocloud.tailb8a678.ts.net")
	     ("domain_resolver" . "tailscale_dns"))
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
       . #((("type" . "direct")
	    ("tag" . "dns_in")
	    ("listen" . "0.0.0.0")
            ("listen_port" . 53)
            ("network" . "udp"))
	   (("type". "mixed")
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
            ("server" . "chikocloud.tailb8a678.ts.net")
            ("server_port" . 7890)
            ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
            ("tls"
             ("enabled" . #f)))
	   (("type" . "direct")
            ("tag" . "out_ruleset"))
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

            ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_direct")))
                   %direct-rules)
	    
	    ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_proxy")))
                   %proxy-rules)
            (("process_name" . #(,@%direct-process))
             ("outbound" . "out_direct"))))
       ("rule_set"
        . #(,@%rule-sets))
       ("final" . "out_direct")
       ("default_domain_resolver" . "dns_direct"))))
  
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %config #:pretty #t) port))))
