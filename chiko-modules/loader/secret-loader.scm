;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules loader secret-loader)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix i18n)
  #:use-module (guix store)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (chiko-modules loader dir-loader)
  #:export (%ssh-keys
	    load-nyapasu))

(define %ssh-keys
  `((plain-file "chiko-ssh.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAOh6siUz1z6TpA5ykI5ftCYLBqV3QHTtECL+ulYLQ+D openpgp:0x1DFD0AED\n")))

(define (load-nyapasu)
  (load (string-append %secretdir "/nyapasu.scm")))
