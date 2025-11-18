;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets container)
  #:use-module (rosenthal)
  #:use-module (gnu services docker)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-container))

(define (make-container)
  (cfgset
   (sys-settings `((services
		    ,(list (service containerd-service-type)
			   (service docker-service-type
				    (docker-configuration
				     (enable-iptables? #f)))))))))
