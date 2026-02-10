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
  #:export (make-swap
	    make-syncthing
	    make-gvfs
	    make-ipfs
	    make-default-file-system-apps
	    make-vps-file-system-apps))

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

(define* (make-ipfs #:key (ipfs "kubo") (gateway-port 8880) (api-port 5001))
  (cfgset
   (home-envs `(("IPFS_GATEWAY" . "http://127.0.0.1/8880")))
   (sys-settings `((packages
		    ,(specifications->packages (list ipfs)))
		   (services
		    ,(list (service ipfs-service-type
				    (ipfs-configuration
				      (package (spec->pkg ipfs))
				      (gateway "/ip4/0.0.0.0/tcp/8880")
				      (api "/ip4/0.0.0.0/tcp/5001")))))))))

(define* (make-default-file-system-apps uuid user #:key (pass-hash "$2a$10$TZcxvm7oleRcyLkl8o8v5emoGHEIXoSczLpmPKSUAEBUqzn4c6Nk."))
  (merge-sets
   (make-swap uuid)
   (make-syncthing user #:config-file (syncthing-config-file
				       (gui-address "0.0.0.0:8384")
				       (gui-user user)
				       (gui-password pass-hash)))
   (make-gvfs)
   (make-ipfs)))

(define (make-vps-file-system-apps uuid user)
  (merge-sets
   (make-swap uuid)
   (make-ipfs)))
