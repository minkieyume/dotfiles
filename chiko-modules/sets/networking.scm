(define-module (chiko-modules sets networking)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu services networking)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-nm-trans-networking
	    make-dhcpcd-networking))

(define network-manager-trans
  (lambda (os)
    (operating-system
     (inherit os)
     (services
      (modify-services (operating-system-user-services os)
  		     (network-manager-service-type
  			config => (network-manager-configuration
                                   (inherit config)
				   (dns "none")
				   (extra-configuration-files
  				  `(("wifi_rand_mac.conf"
  				     ,(plain-file "wifi_rand_mac.conf" "\
  # Generate a random MAC for each network connection and associate the two
  # permanently.
  [connection-mac-randomization]
  ethernet.cloned-mac-address=stable
  wifi.cloned-mac-address=stable\n"))
  				    ("ip6-privacy.conf"
  				     ,(plain-file "ip6-privacy.conf" "\
  # Use IPv6 Privacy Extensions.
  [connection]
  ipv6.ip6-privacy=2\n")))))))))))

(define (make-nftables machine)
  (service nftables-service-type
  	 (nftables-configuration
  	  (ruleset
  	   (local-file (string-append %configdir machine "/nftables.conf"))))))

(define %resolv
  (simple-service 'resolv-service
        	  etc-service-type
        	  `(("resolv.conf" ,(plain-file "resolv.conf" "search tailb8a678.ts.net lan\nnameserver 192.168.8.1\nnameserver 8.8.8.8\nnameserver 1.1.1.1")))))

(define (make-nm-trans-networking machine)
  (cfgset
   (sys-transforms
    (list network-manager-trans))
   (sys-settings
    `((services (,(make-nftables machine)
		 ,%resolv
		 (simple-service 'udp-buffer-size
				 sysctl-service-type
				 '(("net.core.rmem_max" . "7500000")
				   ("net.core.wmem_max" . "7500000")))
		 (simple-service 'ip-forward
				 sysctl-service-type
				 '(("net.ipv4.ip_forward" . "1")
				   ("net.ipv6.conf.all.forwarding" . "1")))))))))

(define (make-dhcpcd-networking machine)
  (cfgset
   (sys-settings
    `((services ((service dhcpcd-service-type
  			  (dhcpcd-configuration
			    (no-hook '("hostname" "resolv.conf"))))
		 (service ntp-service-type)
		 ,(make-nftables machine)
		 (simple-service 'udp-buffer-size
				 sysctl-service-type
				 '(("net.core.rmem_max" . "7500000")
				   ("net.core.wmem_max" . "7500000")))
		 (simple-service 'ip-forward
				 sysctl-service-type
				 '(("net.ipv4.ip_forward" . "1")
				   ("net.ipv6.conf.all.forwarding" . "1")))))))))
