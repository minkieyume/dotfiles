;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets game)
  #:use-module (guix gexp)
  #:use-module (gnu services base)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages game)
  #:export (make-steam
	    make-retroarch
	    make-game
	    make-steam-nvidia
	    make-game-nvidia))

(define (make-steam-cfg sandbox-path)
  (cfgset
   (home-envs `(("GUIX_SANDBOX_HOME" . ,sandbox-path)
		("GUIX_SANDBOX_EXTRA_SHARES" . "$HOME/Downloads:$HOME/Picture/screenshots")))
   (sys-settings `((services ,(list (udev-rules-service 'steam-devices (spec->pkg "steam-devices-udev-rules"))
				    (udev-rules-service 'controller (udev-rule "60-controller-permission.rules" "\
  KERNEL==\"event*\", ATTRS{idVendor}==\"045e\", ATTRS{idProduct}==\"028e\", \
  MODE=\"0660\", GROUP=\"users\""))))))))

(define (make-steam sandbox-path)
  (merge-sets
   (make-steam-cfg sandbox-path)
   (cfgset
    (sys-settings `((packages
		     ,(specifications->packages '("steam"))))))))

(define (make-steam-nvidia sandbox-path)
  (merge-sets
   (make-steam-cfg sandbox-path)
   (cfgset
    (sys-settings `((packages
		     ,(specifications->packages '("steam-nvidia" "nvidia-vaapi-driver"))))))))

(define (make-retroarch)
  (cfgset
   (sys-settings `((packages
		    ,%retroarch-with-extensions)))))

(define (make-game sandbox-path)
  (merge-sets
   (make-steam sandbox-path)
   (make-retroarch)))

(define (make-game-nvidia sandbox-path)
  (merge-sets
   (make-steam-nvidia sandbox-path)
   (make-retroarch)))
