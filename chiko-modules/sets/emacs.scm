;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets emacs)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages emacs)
  #:export (make-emacs))

(define (make-emacs machine)
  (cfgset
   (sys-settings `((packages ,%emacs-packages)))
   (home-settings `((services ,(list (simple-service 'home-emacs
  						     home-shepherd-service-type
  						     (list (shepherd-service
  							    (provision '(emacs-daemon))
  							    (start
  							     #~(make-forkexec-constructor
  								'("emacs" "--fg-daemon")))
  							    (stop
  							     #~(make-forkexec-constructor
  								'("emacsclient" "--eval" "(kill-emacs)"))))))))))
   (home-files `((".authinfo.gpg" ,(local-file (string-append %secretdir "authinfo.gpg")))))
   (home-envs `(("EDITOR" . "emacsclient")
		("VISUAL" . "$EDITOR")
		("ESHELL" . ,(file-append (spec->pkg "fish") "/bin/fish"))))
   (home-mimes '("x-scheme-handler/mailto=emacsclient.desktop"
		 "inode/directory=emacsclient.desktop"
		 "text/plain=emacsclient.desktop"))
   (home-configs
    `(("emacs/init.el"
       ,(computed-substitution-with-inputs "init.el"
      					   (local-file (string-append %configdir "emacs/init.el"))
					   (append
					    (list (local-file (string-append %configdir "emacs") #:recursive? #t)
						  (local-file (string-append %secretdir "epasu") #:recursive? #t))
					    (specs->pkgs "ccls"
      							 "fish"
      							 "python-lsp-server"
      							 "zig-zls"
    							 "fd"
							 "mpv"
							 "ffmpegthumbnailer"
							 "7zip"
							 "imagemagick"
							 "godot"
							 "mediainfo"
							 "vips"
							 "sbcl"))))
      ("emacs/.init-themes.el"
       ,(local-file (string-append %configdir machine "/init-theme.el")))))))
