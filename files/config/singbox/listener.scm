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
        ("level" . "warn")
        ("output" . "singbox.log"))
       ("dns"
         ("servers"
           . #((("type" . "tls")
                 ("tag" . "dns_proxy")
                 ("server" . "1.1.1.1"))
                (("type" . "local")
                  ("tag" . "dns_direct")))))
       ("inbounds"
         . #((("type". "mixed")
               ("tag" . "mixed-in")
               ("listen" . "::")
               ("listen_port" . 7890)
               ("set_system_proxy" . #t))))
       ("outbounds"
         . #((("type" . "vless")
               ("tag" . "vless_out")
               ("detour" . "out_direct")
               ("server" . ,(nyapasu-ref 'sing-box-ip))
               ("server_port" . 7890)
               ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
               ("flow" . "xtls-rprx-vision")
               ("tls"
                 ("enabled" . #t)
                 ("utls"
                   ("enabled" . #t)
                   ("fingerprint" . "firefox"))
                 ("reality"
                   ("enabled" . #t)
                   ("public_key" . ,(nyapasu-ref 'reality-pubic-key))
                   ("short_id"  . "c10a110e"))))
              (("type" . "direct")
                ("tag" . "out_direct")
                ("domain_resolver" . "dns_direct"))
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
                   ("url" . "https://raw.githubusercontent.com/SagerNet/sing-geosite/refs/heads/rule-set/geosite-geolocation-cn.srs"))
                  (("type" . "remote")
                    ("tag" . "geosite-location-!cn")
                    ("url" . "https://raw.githubusercontent.com/SagerNet/sing-geosite/refs/heads/rule-set/geosite-geolocation-!cn.srs"))
                  (("type" . "remote")
                    ("tag" . "geoip-cn")
                    ("url" . "https://raw.githubusercontent.com/SagerNet/sing-geoip/refs/heads/rule-set/geoip-cn.srs"))))
           ("final" . "vless_out")
           ("default_domain_resolver" . "dns_proxy")))))

(define (output-singbox-listener file-name)
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %singbox-listener #:pretty #t) port))))
