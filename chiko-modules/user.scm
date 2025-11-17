;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules user)
  #:use-module (srfi srfi-9)
  #:export (make-nuser
	    <nuser>
	    nuser?
	    nuser-name
	    nuser-account
	    nuser-group
	    nuser-accounts))

(define-record-type <nuser>
  (make-nuser name account group)
  nuser?
  (name nuser-name)
  (account nuser-account)
  (group nuser-group))

(define (nuser-accounts . nuser-list)
  (map nuser-account nuser-list))
