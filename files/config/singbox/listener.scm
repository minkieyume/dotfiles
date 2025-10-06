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

  (define %hysteria-process
    '("ACU.exe"
      "Anon1800.exe"
      "upc.exe"
      "UbisoftConnect.exe"
      "UbisoftGameLauncher.exe"
      "UplayWebCore.exe"))
  
  (define %block-rules
    '((("rule_set" . "geosite-category-ads-all"))))

  (define %direct-rules
    '((("protocol" . "bittorrent"))
      (("protocol" . "stun"))      
      (("rule_set" . "geosite-private"))
      (("rule_set" . "geosite-cn"))
      (("port" . #(3478)))
      (("domain_suffix" . "neboer.site"))
      (("domain_suffix" . #("syncthing.net" "discovery-announce-v4.syncthing.net")))
      (("domain_suffix" . #("lan" "local" "localhost")))
      (("domain_suffix" . "yumieko.com"))
      (("domain_suffix" . #("frp-add.com" "frp-pet.com" "frp-fit.com")))))

  (define %hysteria-rules
    '((("domain_suffix" . "ubisoft.com"))
      (("domain_suffix" . "ubi.com"))
      (("domain_suffix" . "uplay.com"))
      (("domain_suffix" . "ubisoftconnect.com"))))
  
  (define %direct-ips
    '((("rule_set" . "geoip-cn"))
      (("rule_set" . "geoip-private"))))

  (define %proxy-rules
    '((("rule_set" . "geosite-gfw"))
      (("rule_set" . "geosite-category-dev"))
      (("rule_set" . "geosite-stripe"))
      (("domain_suffix" . "manhuagui.com"))
      (("domain_suffix" . "github.com"))
      (("domain_suffix" . "githubusercontent.com"))
      (("ip_cidr" . #("140.82.112.0/20" "140.82.121.0/20" "192.30.252.0/22" "185.199.108.0/22" "143.55.64.0/20")))
      (("domain_suffix" . "boiledscript.com"))
      (("domain_suffix" . "freedesktop.org"))
      (("inbound" . "proxy_in"))))

  (define %proxy-ips
    '((("rule_set" . "geoip-telegram"))))
  
  (define %config
    `(("log"
       ("disabled" . #f)
       ("level" . "debug")
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
	     ("domain" . #("chikoyumemi" "chikocloud" "chikopara" "dreamtwi" "yumemios"))
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
		       ("strategy" . "prefer_ipv4")))
                   %proxy-rules))))
      ("inbounds"
       . #((("type". "mixed")
            ("tag" . "proxy_in")
            ("listen" . "::")
	    ("routing_mark" . 0)
            ("listen_port" . 7890))
           (("type" . "tproxy")
            ("listen" . "::")
            ("listen_port" . 7891)
	    ("routing_mark" . 0)
            ("tag" . "tproxy_in"))))
      ("outbounds"
       . #((("type" . "vless")
            ("tag" . "out_proxy")
            ("server" . "littlewing.yumieko.com")
            ("server_port" . 443)
            ("uuid" . ,(nyapasu-ref 'sing-box-chiko-uuid))
	    ("tls"
	     ("enabled" . #t))
	    ("domain_resolver"
	     ("server" . "dns_cn")
	     ("strategy" . "ipv4_only"))
	    ("transport"
	     ("type" . "ws")
	     ("path" . ,(nyapasu-ref 'ws-transport-path))
	     ("max_early_data" . 512)
	     ("early_data_header_name" . "Sec-WebSocket-Protocol")))
	   (("type" . "hysteria")
	    ("tag" . "out_hysteria")
	    ("server" . "play.yumieko.com")
	    ("server_port" . 443)
	    ("up_mbps" . 100)
	    ("down_mbps" . 100)
	    ("obfs" . ,(nyapasu-ref 'hysteria-obfs))
	    ("auth_str" . ,(nyapasu-ref 'hysteria-password))
	    ("tls"
	     ("enabled" . #t)
	     ("server_name" . "play.yumieko.com")
	     ("alpn" . #("h2", "http/1.1"))
	     ("min_version" . "1.3")))
           (("type" . "direct")
            ("tag" . "out_direct"))
           (("type" . "block")
            ("tag" . "out_block"))))
      ;; ("experimental"
      ;;  ("cache_file"
      ;; 	("enabled" . #t)
      ;; 	("path" . "/var/lib/sing-box/cache.db")))
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

	    (("process_name" . #(,@%hysteria-process))
             ("outbound" . "out_hysteria"))

	    ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_direct")))
                   %direct-ips)
	    
            ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_direct")))
                   %direct-rules)

	    ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_hysteria")))
                   %hysteria-rules)
	    
	    ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_proxy")))
                   %proxy-rules)
	    ,@(map (lambda (rule)
                     `(,@rule
		       ("outbound" . "out_proxy")))
                   %proxy-ips)))
       ("rule_set"
        . #(,@%rule-sets))       
       ("final" . "out_direct")
       ("default_domain_resolver"
	("server" . "dns_cn")
	("strategy" . "prefer_ipv4")))))
  
  (call-with-output-file file-name
    (lambda (port)
      (display (scm->json-string %config #:pretty #t) port))))
