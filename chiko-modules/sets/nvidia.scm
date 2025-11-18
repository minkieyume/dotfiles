;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets nvidia)
  #:use-module (gnu system)
  #:use-module (nonguix transformations)
  #:use-module (nongnu packages nvidia)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-nvidia))

(define (make-nvidia)
  (cfgset
   (sys-transforms
    (list replace-mesa
	  (nonguix-transformation-nvidia)
	  (lambda (os)
	    (operating-system
	     (inherit os)      
	     (kernel-arguments
	      (cons* "modprobe.blacklist=pcspkr,nouveau"
		     "nvidia_drm.modeset=1"
		     (operating-system-user-kernel-arguments os)))))))))
