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
  (cons* "https://substitutes.nonguix.org/"
	 "https://mirrors.sjtug.sjtu.edu.cn/guix"
	 %default-substitute-urls))

(define %authorized-keys
  (cons* (local-file "./files/keys/non-guix.pub")
	 %default-authorized-guix-keys))
