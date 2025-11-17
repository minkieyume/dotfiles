;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules loader guix-loader)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix i18n)
  #:use-module (guix store)
  #:use-module (guix channels)
  #:use-module (chiko-modules loader dir-loader)
  #:export (%default-channels
	    %default-substitute-urls
	    %default-authorized-keys))

(define %default-channels
  (load (string-append %workdir "/channels.scm")))

(define %default-substitute-urls
  (list "https://cache-cdn.guix.moe"
	"https://mirrors.sjtug.sjtu.edu.cn/guix"
	"https://guix.bordeaux.inria.fr"
	"https://bordeaux.guix.gnu.org"
	"https://ci.guix.gnu.org"))

(define %default-authorized-keys
  (cons* (local-file (string-append %pubkeysdir "non-guix.pub"))
	 (local-file (string-append %pubkeysdir "chikoniko.pub"))
	 (local-file (string-append %pubkeysdir "chikoyumemi.pub"))
	 (local-file (string-append %pubkeysdir "guix-moe.pub"))
	 %default-authorized-guix-keys))
