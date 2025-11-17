;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets)
  #:use-module (srfi srfi-9)
  #:use-module (guix records)
  #:export (<cfgset>
	    cfgset
	    cfgset?
	    make-cfgset
	    cfgset-sys-settings
	    cfgset-home-settings
	    cfgset-sys-transforms
	    cfgset-home-transforms
	    cfgset-home-envs
	    cfgset-home-files
	    cfgset-home-configs
	    cfgset-home-desktops
	    cfgset-mcron-jobs
	    cfgset-doas-rules))

(define-record-type* <cfgset>
  cfgset make-cfgset cfgset?
  (sys-settings cfgset-sys-settings (default '()))
  (home-settings cfgset-home-settings (default '()))
  (sys-transforms cfgset-sys-transforms(default '()))
  (home-transforms cfgset-home-transforms (default '()))
  (home-envs cfgset-home-envs (default '()))
  (home-files cfgset-home-files (default '()))
  (home-configs cfgset-home-configs (default '()))
  (home-desktops cfgset-home-desktops (default '()))
  (mcron-jobs cfgset-mcron-jobs (default '()))
  (doas-rules cfgset-doas-rules (default '())))
