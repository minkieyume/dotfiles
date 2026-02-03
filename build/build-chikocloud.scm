;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;Test (load "build/build-chikocloud.scm")
(add-to-load-path (string-append (getcwd)))
(use-modules (chiko-modules machine chikocloud)
	     (ice-9 pretty-print)
	     (gnu system)
	     (gnu services))

(define os
  (make-chikocloud-os))

os
