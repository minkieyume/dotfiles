;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets hardware)
  #:use-module (rosenthal)
  #:use-module (gnu services base)
  #:use-module (chiko-modules packages hardware)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-hdtoolkit))

(define (make-hdtoolkit)
  (cfgset
   (sys-settings `((packages
		    ,%hd-toolkit)
		   (services
		    ,(list
		      (udev-rules-service 'hdparm
					  (file->udev-rule
					   "90-hdparm.rules"
					   (mixed-text-file "90-hdparm.rules"
							    #~(string-append
							       "ACTION==\"add\", SUBSYSTEM==\"block\",KERNEL==\"sd[a-z]\","
							       "RUN+=\"" #$(file-append (spec->pkg "hdparm") "/sbin/hdparm") " -B 127 -S 120 /dev/%k\""))))))))))
