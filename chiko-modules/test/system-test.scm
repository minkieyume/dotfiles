;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; use guix repl and then (load "chiko-modules/test/system-test.scm") for test in dotfiles

(add-to-load-path (string-append (getcwd)))

(use-modules (chiko-modules system)
	     (chiko-modules sets)
	     (gnu)
	     (gnu system)
	     (gnu home)
	     (language tree-il))

;; ((compose tree-il->scheme syntax->datum macroexpand) '(make-system (lambda (x) x) (services (make-doas) (make-mcron))))

;; (define ssh-tests
;;   (list
;;    (make-ssh ('("deploy1"
;; 		(plain-file "ssh.pub" "ssh-ed25519 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AXX"))
;; 	      '("deploy2"
;; 		(plain-file "ssh.pub" "ssh-ed25519 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AXX"))))
   
;;    (make-ssh ('("deploy2"
;; 		(plain-file "ssh.pub" "ssh-ed25519 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA AXX")))
;; 	     ("ENV1" "ENV2" "ENV3"))))

;; (display (object->string ssh-tests))

;; (define home-test
;;   (make-home-service ("chiko1") ((home-environment))))

;; (display (object->string home-test))

;; (define os
;;   (make-system ((lambda (x) x))
;; 	       ((locale "zh_CN.utf8")
;; 		(timezone "Asia/Singapore")
;; 		(keyboard-layout (keyboard-layout "us"))
;; 		(host-name "chikocloud")
;; 		(bootloader (bootloader-configuration
;; 			     (bootloader grub-bootloader)
;; 			     (targets (list "/dev/sda"))))

;; 		(file-systems (append (list (file-system
;; 					     (mount-point "/")
;; 					     (device (uuid "5e270b1a-b415-4956-9aff-ae400e2ff3f4"))
;; 					     (type "ext4"))) %base-file-systems))
;; 		(services (list (make-doas) (make-mcron (job "* * * * * echo Hello")))))))

(define test-config
  (cfgset
   (home-settings `((packages ((spec->pkg "emacs") (spec->pkg "ripgrep") (spec->pkg "git")))
		    (services ((service home-env-service)))))
   (sys-settings `((locale "zh_CN.utf8")
		   (timezone "Asia/Singapore")
		   (keyboard-layout (keyboard-layout "us"))
		   (host-name "chikocloud")
		   (bootloader (bootloader-configuration
				(bootloader grub-bootloader)
				(targets (list "/dev/sda"))
				(keyboard-layout keyboard-layout)))
		   (file-systems (append (list (file-system
						(mount-point "/")
						(device (uuid "5e270b1a-b415-4956-9aff-ae400e2ff3f4"))
						(type "ext4"))) %base-file-systems))
		   (services (list (simple-service 'test-service
						   etc-service-type `(("issue" . (plain-file "issue" "Test Issue"))))))))))

(define os
  (make-system test-config))

(display (string-append (object->string os) "\n"))
