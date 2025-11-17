;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; use guix repl and then (load "chiko-modules/test/set-test.scm") for test in dotfiles

(add-to-load-path (string-append (getcwd)))

(use-modules (chiko-modules sets)
	     (chiko-modules loader dir-loader)
	     (chiko-modules loader set-loader)
	     (rosenthal)
	     (gnu system)
	     (gnu home)
	     (language tree-il))
(display "--------SET TEST--------\n")

(define set1
  (cfgset
   (home-settings `((packages ((spec->pkg "emacs") (spec->pkg "ripgrep") (spec->pkg "git")))
		    (services ((service home-env-service)))))
   (home-envs `(("EDITOR" . "emacsclient")
		("VISUAL" . "emacsclient")
		("PAGER" . "less")))
   (home-configs
   `(("emacs" . ,(plain-file "emacs-config.el" "(setq inhibit-startup-screen t)"))
     ("git" . ,(plain-file "gitconfig" "[user]\n\tname = Minkie Yume\n\temail ="))))
   (mcron-jobs
   `((job "0 5 * * *")))))

(define set2
  (cfgset
   (home-settings `((packages ((spec->pkg "guile")))
		    (services ((service file-sync-service)))))
   (home-envs `(("FLYER" . "HELP")))))

(display (string-append (object->string (merge-sets set1 set2)) "\n"))
