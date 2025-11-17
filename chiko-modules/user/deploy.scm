;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules user deploy)
  #:use-module (srfi srfi-9)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:export (make-deploy))

(define (make-deploy)
  (make-nuser
   (name "deploy")
   (account (user-account
	     (name "deploy")
	     (comment "Deploy")
	     (group "users")
	     (home-directory "/home/deploy")
	     (supplementary-groups '("wheel"))))))
