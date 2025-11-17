;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules user minkieyume)
  #:use-module (srfi srfi-9)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:export (make-minkieyume))

(define (make-minkieyume)
  (make-nuser
   (name "minkieyume")
   (account (user-account
	     (name "minkieyume")
	     (comment "Minkieyume")
	     (password "$6$4QkvhBIch2jaueTp$h7P.Q.JlV3iT7xnoUyXoo0obiOsnSxmP8Rscv2PpF1YhP7I6Sp3/CN5VddDSxGqOWfzo0D.2yeP/Km4oCsOvm1")
	     (group "users")
	     (shell (file-append (spec->pkg "fish") "/bin/fish"))
	     (home-directory "/home/minkieyume")
	     (supplementary-groups '("users" "wheel" "netdev" "audio" "video" "docker"))))))
