;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets)
  #:use-module (srfi srfi-9)
  #:use-module (guix records)
  #:use-module (chiko-modules utils)
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
	    cfgset-home-mimes
	    cfgset-mcron-jobs
	    cfgset-doas-rules
	    cfgset-user-list
	    cfgset-autoload-kernel-modules
	    merge-sets))

(define-record-type* <cfgset>
  cfgset make-cfgset cfgset?
  (sys-settings cfgset-sys-settings (default '()))
  (home-settings cfgset-home-settings (default '()))
  (sys-transforms cfgset-sys-transforms (default '()))
  (home-transforms cfgset-home-transforms (default '()))
  (home-envs cfgset-home-envs (default '()))
  (home-files cfgset-home-files (default '()))
  (home-configs cfgset-home-configs (default '()))
  (home-desktops cfgset-home-desktops (default '()))
  (home-mimes cfgset-home-mimes (default '()))
  (mcron-jobs cfgset-mcron-jobs (default '()))
  (doas-rules cfgset-doas-rules (default '()))
  (user-list cfgset-user-list (default '()))
  (autoload-kernel-modules cfgset-autoload-kernel-modules (default '())))

(define (merge-sets . sets)
  (if (null? (cdr sets))
      (car sets)
      (merge-set (car sets)
		 (apply merge-sets (cdr sets)))))

(define (merge-set set1 set2)
  (cfgset
   (sys-settings (merge-config (cfgset-sys-settings set1) (cfgset-sys-settings set2)))
   (home-settings (merge-config (cfgset-home-settings set1) (cfgset-home-settings set2)))
   (home-envs (append (cfgset-home-envs set1) (cfgset-home-envs set2)))
   (home-files (append (cfgset-home-files set1) (cfgset-home-files set2)))
   (home-configs (append (cfgset-home-configs set1) (cfgset-home-configs set2)))
   (home-desktops (append (cfgset-home-desktops set1) (cfgset-home-desktops set2)))
   (home-mimes (append (cfgset-home-mimes set1) (cfgset-home-mimes set2)))
   (mcron-jobs (append (cfgset-mcron-jobs set1) (cfgset-mcron-jobs set2)))
   (doas-rules (append (cfgset-doas-rules set1) (cfgset-doas-rules set2)))
   (sys-transforms (append (cfgset-sys-transforms set1) (cfgset-sys-transforms set2)))
   (home-transforms (append (cfgset-home-transforms set1) (cfgset-home-transforms set2)))
   (user-list (append (cfgset-user-list set1) (cfgset-user-list set2)))
   (autoload-kernel-modules (append (cfgset-autoload-kernel-modules set1)
				    (cfgset-autoload-kernel-modules set2)))))
