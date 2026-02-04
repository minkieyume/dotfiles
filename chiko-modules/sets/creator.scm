;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets creator)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module ((chiko services networking) #:prefix chiko:)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages creator)
  #:export (make-singbox-listener
	    make-singbox-streamer
	    make-krita
	    make-creator))

(define %sing-box-listener
  (local-file (string-append %configdir "singbox/listener.scm")))

(define %sing-box-listener-config-file
  (computed-file "sing-box.json"
		 (with-extensions (map specification->package '("guile-json@4"))
				  #~(begin
				      (primitive-load #$%nyapasu-script)
				      (primitive-load #$%sing-box-listener)
				      (sing-box-listener #$output)))))

;;安装八音盒音乐接收器
(define (make-singbox-listener)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("sing-box")))
		   (services
		    ,(list
		      (service chiko:sing-box-service-type
  			       (chiko:sing-box-configuration
  				(sing-box (spec->pkg "sing-box"))
  				(config-file %sing-box-listener-config-file)
  				(tproxy-nft-config-file (local-file (string-append %configdir "singbox/singbox-tproxy.nft")))
  				(auto-start? #t)))))))))

(define %sing-box-streamer
  (local-file "../files/config/singbox/streamer.scm"))

(define %sing-box-streamer-config-file
  (computed-file "sing-box.json"
		 (with-extensions (map specification->package '("guile-json@4"))
				  #~(begin
				      (primitive-load #$%nyapasu-script)
				      (primitive-load #$%sing-box-streamer)
				      (sing-box-streamer #$output)))))

;;安装八音盒音乐推流器
(define (make-singbox-streamer)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("sing-box")))
		   (services
		    ,(list
		      (service chiko:sing-box-service-type
  			       (chiko:sing-box-configuration
  				(sing-box (spec->pkg "sing-box"))
  				(config-file %sing-box-streamer-config-file)
  				(auto-start? #t)))))))))

(define (make-krita)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("krita")))))
   (home-desktops `(("org.kde.krita.desktop" "krita")))))

(define (make-creator)
  (merge-sets
   (make-singbox-listener)
   (make-krita)
   (cfgset
    (sys-settings `((packages
		     ,%chiko-creator))))))
