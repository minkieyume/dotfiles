;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets server)
  #:use-module (rosenthal)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services rsync)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services dbus)
  #:use-module (chiko-modules packages server)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-rsync
	    make-base-server
	    make-vps))

(define (make-rsync . modules)
  (cfgset
   (sys-settings `((services
		    ,(list (service rsync-service-type
  				    (rsync-configuration
  				     (modules modules)))))))))

(define (make-postgresql)
  (service postgresql-service-type
	   (postgresql-configuration
	    (postgresql (spec->pkg "postgresql@15"))
	    (allow-login? #t)
	    (config-file (postgresql-config-file
			  (log-destination "stderr")
			  (hba-file
			   (plain-file "pg_hba.conf"
				       (string-join
					(list ""
					      "local	all	all			trust"
					      "host	all	all	127.0.0.1/32 	md5"
					      "host	all	all	::1/128 	md5"
					      "host  all     all     172.17.0.0/16   md5")
					"\n")))
			  (extra-config
			   '(("listen_addresses"           "*"))))))))

(define* (make-base-server #:key
			   (sys-base %base-services)
			   (home-base %base-home-services))
  (cfgset
   (home-settings `((services
		     ,home-base)))
   (sys-settings `((packages
		    ,%server-toolkit)
		   (services
		    ,(append (list (service opendht-service-type
					    (opendht-configuration
					      (peer-discovery? #t)))
				   (service dbus-root-service-type)
				   (service elogind-service-type)
				   (service redis-service-type
					    (redis-configuration
					      (bind "0.0.0.0")))
				   (make-postgresql))
			     sys-base))))))

(define* (make-vps #:key
		   (sys-base %base-services)
		   (home-base %base-home-services))
  (merge-sets (make-base-server sys-base home-base)
	      (cfgset
	       (sys-settings `((initrd-modules ("virtio_scsi" "virtio_pci")))))))
