;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets appstore)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-flatpak
	    make-flatpak-desktops
	    %default-flatpak-desktops))

(define %default-flatpak-desktops
  '("org.mapeditor.Tiled.desktop"
    "org.kde.digikam.desktop"
    "org.localsend.localsend_app.desktop"
    "com.calibre_ebook.calibre.desktop"
    "com.orama_interactive.Pixelorama.desktop"
    "com.qq.QQ.desktop"))

(define (make-flatpak-desktops . apps)
  (simple-service 'flatpak-desktop
                  home-files-service-type
		  `(,@(map (lambda (desktop-file)
                             `(,(string-append ".local/share/applications/" desktop-file)
			       ,(local-file (string-append %desktopdir desktop-file))))
			   apps))))

(define (make-flatpak desktop-service)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("flatpak" "flatpak-xdg-utils")))))
   (home-settings `((services
		     ,(list desktop-service))))))
