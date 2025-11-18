;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets filesystem)
  #:use-module (guix gexp)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu services networking)
  #:use-module (gnu services syncthing)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages desktop)
  #:export (make-swap
	    make-syncthing
	    make-gvfs
	    make-ipfs
	    make-default-file-system-apps))

(define (make-swap u-uid)
  (cfgset
   (sys-transforms (list (lambda (os)
			   (operating-system
			    (inherit os)
			    (swap-devices
			     (list (swap-space
				    (target (uuid u-uid))
				    (discard? #t))))))))))

(define* (make-syncthing user #:key (config-file #f))
  (cfgset
   (sys-settings `((services
		    ,(if config-file
			 (list (service syncthing-service-type
  					(syncthing-configuration (user user)
								 (config-file config-file))))
			 (list (service syncthing-service-type
  					(syncthing-configuration (user user))))))))))

(define (make-gvfs)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("gvfs" "glib:bin")))
		   (services
		    ,(list (service gvfs-service-type)))))))

(define* (make-ipfs #:key (ipfs "kubo") (gateway "/ip4/0.0.0.0/tcp/8880") (api "/ip4/0.0.0.0/tcp/5001"))
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages (list ipfs)))
		   (services
		    ,(list (service ipfs-service-type
				    (ipfs-configuration
				     (package (spec->pkg "kubo"))
				     (gateway "/ip4/0.0.0.0/tcp/8880")
				     (api "/ip4/0.0.0.0/tcp/5001")))))))))

(define (make-default-file-system-apps uuid user)
  (merge-sets
   (make-swap uuid)
   (make-syncthing user)
   (make-gvfs)
   (make-ipfs)))
