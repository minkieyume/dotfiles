;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets secret)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module (gnu home services gnupg)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages desktop)
  #:export (make-gpg-agent
	    make-keepassxc))

(define* (make-gpg-agent #:key
			 (pinetry "pinentry-emacs")
			 (extra-options '("allow-emacs-pinentry"
					  "allow-loopback-pinentry")))
  (cfgset
   (home-settings `((services
		     ,(list (service home-gpg-agent-service-type
				     (home-gpg-agent-configuration
				       (pinentry-program
					(file-append (spec->pkg pinetry) (string-append "/bin/" pinetry)))
				       (ssh-support? #t)
				       (extra-content (string-join extra-options "\n"))))))))
   (sys-settings `((packages ,(specifications->packages (list pinetry)))))))

(define (make-keepassxc)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("libsecret"
						 "keepassxc")))))))
