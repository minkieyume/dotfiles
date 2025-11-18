;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets daily)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages desktop)
  #:export (make-icecat
	    make-mu-isync
	    make-daily))

(define* (make-icecat #:key (icecat "librewolf"))
  (cfgset
   (home-settings `((packages ,(list (spec->pkg icecat)))))
   (home-envs `(("MOZ_ENABLE_WAYLAND" . "1")))))

(define (make-mu-isync)
  (cfgset
   (home-settings `((packages ,(specifications->packages '("mu" "isync")))))
   (home-files `((".mbsyncrc"
  		,(local-file (string-append %configdir "mbsyncrc.conf")))))
   (mcron-jobs `((job '(next-minute (range 0 60 3))
		      "mbsync -a > /tmp/mbsync.log 2>&1")))))

(define (make-element)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("element-desktop")))))
   (home-desktops `(("element-desktop.desktop" "element-desktop")))))

(define (make-jami)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("jami")))))))

(define* (make-daily #:key
		     (browser (make-icecat))
		     (mail-client (make-mu-isync)))
  (merge-sets browser mail-client (make-element) (make-jami)))
