;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules user)
  #:use-module (srfi srfi-9)
  #:use-module (guix records)
  #:export (make-nuser
	    <nuser>
	    nuser
	    nuser?
	    nuser-name
	    nuser-make-home?
	    nuser-account
	    nuser-group
	    nuser-accounts
	    nuser-groups
	    nuser-make-home-names))

(define-record-type* <nuser>
  nuser make-nuser nuser?
  (name nuser-name (default ""))
  (make-home? nuser-make-home? (default #f))
  (account nuser-account (default '()))
  (group nuser-group (default '())))

(define (nuser-make-home-names . nuser-list)
  (map nuser-name
       (filter (lambda (x) (nuser-make-home? x))
	       nuser-list)))

(define (nuser-accounts . nuser-list)
  (filter (lambda (x) (not (null? x)))
	  (map nuser-account nuser-list)))

(define (nuser-groups . nuser-list)
  (filter (lambda (x) (not (null? x)))
	  (map nuser-group nuser-list)))
