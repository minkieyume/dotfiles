;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules packages game)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%retroarch-with-extensions))

(define %retroarch-with-extensions
  (specifications->packages
   '("retroarch"
     "retroarch-joypad-autoconfig"
     "retroarch-assets"
     "libretro-database"
     "libretro-slang-shaders"
     "libretro-lowresnx")))
