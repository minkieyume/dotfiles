;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets)
  #:use-module (srfi srfi-9)
  #:export (make-cfgset*
	    make-cfgset
	    <cfgset>
	    cfgset?
	    cfgset-sys-settings
	    cfgset-home-settings
	    cfgset-home-envs
	    cfgset-home-files
	    cfgset-home-configs
	    cfgset-home-desktops
	    cfgset-mcron-jobs
	    cfgset-doas-rules))

(define (make-cfgset*
         #:key
         (sys-settings '())
         (home-settings '())
         (home-envs '())
         (home-files '())
         (home-configs '())
         (home-desktops '())
         (mcron-jobs '())
         (doas-rules '())  
         (sys-transforms '())
         (home-transforms '()))
  (make-cfgset
   sys-settings
   home-settings
   sys-transforms
   home-tramsforms
   home-envs
   home-files
   home-configs
   home-desktops
   mcron-jobs
   doas-rules))

(define-record-type <cfgset>
  (make-cfgset
   sys-settings
   home-settings
   sys-transforms
   home-tramsforms
   home-envs
   home-files
   home-configs
   home-desktops
   mcron-jobs
   doas-rules)
  cfgset?
  (sys-settings cfgset-sys-settings)
  (home-settings cfgset-home-settings)
  (sys-transforms cfgset-sys-transforms)
  (home-tramsforms cfgset-home-transforms)
  (home-envs cfgset-home-envs)
  (home-files cfgset-home-files)
  (home-configs cfgset-home-configs)
  (home-desktops cfgset-home-desktops)
  (mcron-jobs cfgset-mcron-jobs)
  (doas-rules cfgset-doas-rules))
