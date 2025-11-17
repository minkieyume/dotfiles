;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules user root)
  #:use-module (srfi srfi-9)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (rosenthal)
  #:use-module (chiko-modules user)
  #:export (make-root))

(define (make-root)
  (nuser
   (name "root")
   (account (user-account
	      (inherit %root-account)
	      (password #f)))))
