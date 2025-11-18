;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets guix)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader guix-loader)
  #:use-module (chiko-modules sets)
  #:export (make-guix))

(define (make-guix extra-options tmpdir)
  (cfgset
   (sys-transforms
    (list (lambda (os)
	    (operating-system
	     (inherit os)
	     (services
	      (modify-services (operating-system-user-services os)
			       (guix-service-type
  				config => (guix-configuration
					   (inherit config)
					   (substitute-urls %default-substitute-urls)
					   (channels %default-channels)
					   (authorized-keys %default-authorized-keys)
					   (discover? #t)
					   (extra-options extra-options)
					   (tmpdir tmpdir)))))))))))
