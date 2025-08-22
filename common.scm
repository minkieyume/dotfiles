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
(define %chiko-channels
  (cons* (channel
          (name 'chikochannel)
          (url "https://github.com/minkieyume/chiko-guix-channel.git")
          (branch "main")
          (introduction
           (make-channel-introduction
            "58c72b83e045c90bcef7edcc7b90b9b5fe875b03"
            (openpgp-fingerprint
             "F655 AB8D 8F94 0E9F 569C E97D 5DBC D441 1818 2F31"))))
	 (channel
	  (name 'rosenthal)
	  (url "https://codeberg.org/hako/rosenthal.git")
	  (branch "trunk")
	  (introduction
	   (make-channel-introduction
	    "7677db76330121a901604dfbad19077893865f35"
	    (openpgp-fingerprint
	     "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7"))))
	 (channel
	  (name 'nonguix)
	  (url "https://gitlab.com/nonguix/nonguix")
	  ;; Enable signature verification:
	  (introduction
	   (make-channel-introduction
	    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
	    (openpgp-fingerprint
	     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
	 (channel
	  (name 'guixcn)
	  (url "https://codeberg.org/guixcn/guix-channel.git")
	  (introduction
	   (make-channel-introduction
            "993d200265630e9c408028a022f32f34acacdf29"
            (openpgp-fingerprint
             "7EBE A494 60CE 5E2C 0875  7FDB 3B5A A993 E1A2 DFF0"))))
	 %default-channels))

;;预编译包链接
(define %chiko-substitute-urls
  (cons* "https://substitutes.nonguix.org/"
	 "https://mirrors.sjtug.sjtu.edu.cn/guix"
	 %default-substitute-urls))

(define %authorized-keys
  (cons* (local-file "./files/keys/non-guix.pub")
	 %default-authorized-guix-keys))
