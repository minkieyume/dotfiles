;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets video)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages desktop)
  #:export (make-video
	    make-video-nvidia
	    make-video-terminal
	    make-v4l))

(define (make-video-terminal)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("ffmpeg")))))))

(define (make-video)
  (merge-sets
   (make-v4l)
   (cfgset
    (sys-settings `((packages
		     ,(specifications->packages '("mpv" "ffmpeg" "obs" "kodi-wayland"))))))))

(define (make-video-nvidia)
  (merge-sets
   (make-v4l)
   (cfgset
    (sys-settings `((packages
		     ,(specifications->packages '("mpv-nvidia" "obs-nvidia" "kodi-wayland" "ffmpeg"))))))))

(define (make-v4l)
  (cfgset
   (autoload-kernel-modules '("uvcvideo" "videodev" "v4l2loopback"))
   (sys-settings `((packages
		    ,(specifications->packages '("v4l-utils")))
		   (kernel-loadable-modules
		    ,(specifications->packages '("v4l2loopback-linux-module")))))
   (sys-transforms
    (list (lambda (os)
	    (operating-system
	     (inherit os)
	     (kernel-arguments
	      (cons* "v4l2loopback.devices=1"
  		     "v4l2loopback.exclusive_caps=1"
  		     "v4l2loopback.card_label=Virtual Camera"
		     (operating-system-user-kernel-arguments os)))))))))
