;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets work)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-gnucash))

(define (make-gnucash)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("gnucash" "gnucash:doc" "gnucash:python")))))))
