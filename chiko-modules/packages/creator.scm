;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules packages creator)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%chiko-creator))

(define %chiko-creator
  (specifications->packages
   '("godot"
     "hugo"
     "texlive"
     "texlive-pdflatexpicscale"
     "krita"
     "inkscape"
     "libwacom"
     "lmms")))
