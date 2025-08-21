(use-modules (json))

(define %singbox-listener
  (let ((%direct-process
          '("rclone"
             "steam"
             "sing-box"
             "wineserver"
             "syncthing"
             "yggdrasil"
             "smartdns"
             "PHOGS.exe"))
         (%direct-rules
           '((("protocol" . "bittorrent"))
              (("rule_set" . "geosite-location-cn"))
              (("domain_suffix"
                 . #("yumieko.com"
                     "frp-add.com"
                     "frp-pet.com"
                     "frp-fit.com")))
              (("type" . "logical")
                ("mode" . "and")
                ("rules"
                  . #((("rule_set" . "geosite-location-!cn")
                        ("invert" . #t))
                       (("rule_set" . "geoip-cn"))))))))
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
           ("default_domain_resolver" . "cloudflare")))))

(define (output-singbox-listener file-name)
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %singbox-listener #:pretty #t) port))))
