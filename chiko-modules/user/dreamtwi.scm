;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules user dreamtwi)
  #:use-module (srfi srfi-9)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (rosenthal)
  #:use-module (chiko-modules user)
  #:export (make-dreamtwi))

(define (make-dreamtwi)
  (nuser
   (name "dreamtwi")
   (make-home? #t)
   (account (user-account
	     (name "dreamtwi")
	     (comment "Dreamtwi")
	     (password "$6$dreamtwi-best$a9/vb1nJ/rDiP3e7lP/T633w92o4Iekbc4UvDWKjGsIUCaWTqthtjnrbRhVzm.je8op8PSBVuANuQnkgONvEK1")
	     (group "users")
	     (shell (file-append (spec->pkg "fish") "/bin/fish"))
	     (home-directory "/home/dreamtwi")
	     (supplementary-groups '("users" "wheel" "netdev" "audio" "video" "docker"))))
   (group (user-group
	   (name "dreamtwi")))))
