(use-modules (json))
(load "../../../reconfigure/nyapasu.scm")

(define %singbox-listener
  (let ((%direct-process
          '("rclone"
             "steam"
             "sing-box"
             "wineserver"
             "syncthing"
             "yggdrasil"
             "smartdns"))
         (%direct-rules
           '((("protocol" . "bittorrent"))
              (("rule-set" . "geosite-location-cn"))
              (("type" . "logical")
                ("mode" . "and")
                ("rules"
                  . #((("rule-set" . "geosite-location-!cn")
                        ("invert" . #t))
                       (("rule-set" . "geoip-cn"))))))))
    `(("log"
        ("level" . "warn")
        ("output" . "singbox.log"))
       ("dns"
         ("servers"
           . #((("type" . "tls")
                 ("tag" . "DNS Proxy")
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
               ("server" . ,(nyapasu-ref 'sing-box-ip))
               ("server_port" . 7890)
               ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
               ("flow" . "xtls-rprx-vision")
               ("tls"
                 ("enabled" . #t)
                 ("reality"
                   ("enabled" . #t)
                   ("public_key" . ,(nyapasu-ref 'reality-pubic-key))
                   ("short_id"  . "c10a110e"))))
              (("type" . "direct")
                ("tag" . "out_direct")
                ("doamin_resolver" . "dns_direct"))
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
           ("default_domain_resolver" . "dns_direct")))))

(call-with-output-file "../../../reconfigure/listener.json"
  (lambda (port)
    (display (scm->json-string %singbox-listener #:pretty #t) port)))
