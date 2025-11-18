;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules loader dir-loader)
  #:export (%workdir
	    %filedir
	    %configdir
	    %secretdir
	    %desktopdir
	    %pubkeysdir
	    %dotfilesdir
	    get-machine-config-dir))

;; Base Directories
(define %workdir (getcwd))
(define %filedir (string-append %workdir "/files/"))
(define %secretdir (string-append %workdir "/secret/"))

;; Files
(define %configdir (string-append %filedir "config/"))
(define %desktopdir (string-append %filedir "desktop/"))
(define %pubkeysdir (string-append %filedir "keys/"))

;; ConfigDir
(define %dotfilesdir (string-append %configdir "dotfiles/"))

;; Methods
(define (get-machine-configdir machine-name)
  (string-append %configdir machine-name "/"))
