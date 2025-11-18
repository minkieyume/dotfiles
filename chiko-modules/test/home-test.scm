;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; use guix repl and then (load "chiko-modules/test/home-test.scm") for test in dotfiles

(add-to-load-path (string-append (getcwd)))

(use-modules (chiko-modules home)
	     (chiko-modules loader dir-loader)
	     (chiko-modules sets)
	     (gnu system)
	     (gnu home)
	     (language tree-il))
(display "--------HOME TEST--------\n")

;; (define home
;;   (make-home ((lambda (x) x))
;; 	     ((packages (specifications->packages '("neofetch" "curl")))
;; 	      (services (list (make-home-env '("ENV1" . "ENV2"))
;; 			      (make-home-dotfiles %dotfilesdir)
;; 			      (make-home-desktop ("element-desktop" "element-desktop"))
;; 			      (make-home-mime "app1" "app2" "app3" "app4"))))))

(define home-set
  (cfgset
   (home-settings `((packages ((spec->pkg "neofetch") (spec->pkg "curl")))))
   (home-envs `(("ENV1" . "ENV2") ("EDITOR" . "emacs")))
   (home-desktops '(("element-desktop" "element-desktop")))
   (home-mimes '("app1" "app2" "app3" ))))

(define home
  (make-home home-set))

(display (string-append (object->string home) "\n"))
