;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;Test (load "build/build-chikoyumemi.scm")
(add-to-load-path (string-append (getcwd)))
(use-modules (chiko-modules machine chikoyumemi)
	     (ice-9 pretty-print)
	     (gnu system)
	     (gnu services))

(define os
  (make-chikoyumemi-os))

(display "-------- CHIKO YUMEMI --------")
(newline)
(display "Check Service:")
(newline)
(pretty-print (operating-system-user-services os))

os
