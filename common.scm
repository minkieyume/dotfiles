(use-modules (srfi srfi-1)
	     (srfi srfi-26)
	     (ice-9 match)
	     (ice-9 popen)
	     (ice-9 textual-ports)
	     (guix diagnostics)
	     (guix i18n)
	     (guix store)
	     (guix channels)
	     (nonguix transformations)
	     (rosenthal))

;; 频道配置
(load "./channels.scm")

;;预编译包链接
(define %chiko-substitute-urls
  (list "https://mirrors.sjtug.sjtu.edu.cn/guix"
	"https://bordeaux.guix.gnu.org"
	"https://ci.guix.gnu.org"
	"https://substitutes.nonguix.org"))

(define %chiko-authorized-keys
  (cons* (local-file "./files/keys/non-guix.pub")
	 (local-file "./files/keys/chikoniko.pub")
	 %default-authorized-guix-keys))

(define %chiko-ssh-key
  (plain-file "chiko-ssh.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAOh6siUz1z6TpA5ykI5ftCYLBqV3QHTtECL+ulYLQ+D openpgp:0x1DFD0AED\n"))
