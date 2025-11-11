(use-modules (gnu)
(gnu system)
(gnu system privilege)
(srfi srfi-26)
(ice-9 match)
(gnu services sysctl)
(gnu services admin)
(gnu services configuration)
(rosenthal)
(gnu services networking)
(gnu packages version-control)
(chiko services doas)
(rosenthal services networking)
(gnu services docker))

(define %this-dir
  (let ((f (current-filename)))
    (if f (dirname f) (getcwd))))
(define (os-sys-app os)
  (operating-system
   (inherit os)
   (initrd-modules (append (list ) (operating-system-initrd-modules os)))
   (kernel-loadable-modules (append (specifications->packages (list )) (operating-system-kernel-loadable-modules os)))
   (kernel-arguments (append (list ) (operating-system-kernel-arguments os)))
   (privileged-programs (append (list ) (operating-system-privileged-programs os)))
   (packages (append (list (spec->pkg "hello")) (specifications->packages (list "btop" "curl" "neofetch"
"fish"
"openssl"
"rsync"
"cryptsetup"
"bind:utils"
"tcpdump"
"git"
"opendoas"
"gnupg"
"kubo"
"unzip"
"gnunet"
"gnunet-scheme")) (operating-system-packages os)))
   (services
    (append (list (service pam-limits-service-type
  	 (list
          (pam-limits-entry "*" 'both 'nofile 100000)))
;; https://github.com/quic-go/quic-go/wiki/UDP-Buffer-Sizes
(simple-service 'udp-buffer-size
  sysctl-service-type
  '(("net.core.rmem_max" . "7500000")
     ("net.core.wmem_max" . "7500000")))
(simple-service 'ip-forward
  sysctl-service-type
  '(("net.ipv4.ip_forward" . "1")
     ("net.ipv6.conf.all.forwarding" . "1")))
(service doas-service-type
  (doas-configuration
    (rules
      (list (doas-rule
  (permit #t)
  (user ":wheel")
  (options '("persist" "keepenv")))
(doas-rule
  (permit #t)
  (user ":wheel")
  (options '("persist"
             "setenv { http_proxy https_proxy HOME=/root XDG_CACHE_HOME=/root/.cache PATH=/run/setuid-programs:/root/.config/guix/current/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin INFOPATH=/root/.config/guix/current/share/info:/run/current-system/profile/share/info GIT_EXEC_PATH=/root/.guix-profile/libexec/git-core}"))
  (as-target "root"))))))
(service ipfs-service-type
         (ipfs-configuration
	   (package (spec->pkg "kubo"))
           (gateway "/ip4/0.0.0.0/tcp/8880")
           (api "/ip4/0.0.0.0/tcp/5001")))
(service tailscale-service-type)
(service containerd-service-type)
(service docker-service-type
  (docker-configuration
    (enable-iptables? #f)))) (operating-system-user-services os)))))
