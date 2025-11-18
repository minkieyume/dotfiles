;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules packages icecat)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%icecat-extensions))

(define %icecat-extensions
  (specifications->packages
   '("icecat-l10n:zh-CN"
     "adaptive-tab-bar-colour-icecat"
     "livemarks-icecat"
     "miniflux-injector-icecat"
     "ohmyech-icecat"
     "privacy-redirect-icecat"
     "ublock-origin-icecat"
     "keepassxc-browser-icecat")))
