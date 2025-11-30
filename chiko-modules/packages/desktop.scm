;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules packages desktop)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%font-packages
	    %fcitx-packages
	    %xdg-packages
	    %niri-packages
	    %desktop-packages))

(define %font-packages
  (specifications->packages
   '("font-awesome"
     "font-adobe-source-serif"
     "font-google-noto"
     "font-google-noto-sans-cjk"
     "font-google-noto-serif-cjk"
     "font-google-noto-emoji"
     "font-victor-mono"
     "font-sarasa-gothic")))

(define %fcitx-packages
  (specifications->packages
   '("fcitx5"
     "fcitx5-configtool"
     "fcitx5-gtk"
     "fcitx5-gtk4")))

(define %xdg-packages
  (specifications->packages
   '("xdg-desktop-portal"
     "xdg-desktop-portal-gnome"
     "xdg-desktop-portal-gtk"
     "xdg-utils"
     "xdg-dbus-proxy"
     "xkeyboard-config")))

(define %niri-packages
  (specifications->packages
   '("niri"
     "wayland"
     "wayland-protocols"
     "egl-wayland"
     "wl-clipboard"
     "imv"
     "rofi"
     "wireplumber"
     "xwayland-satellite"
     "foot"
     "light"
     "swaylock-effects"
     "wireplumber"
     "xwayland-satellite"
     "helvum"
     "pavucontrol"
     "hicolor-icon-theme")))

(define %graphics-packages
  (specifications->packages
   '("libxkbcommon"
     "alsa-lib"
     "libgudev"
     "vulkan-loader"
     "mesa-utils"
     "glfw"
     )))

(define %desktop-packages
  (append %font-packages
	  %fcitx-packages
	  %xdg-packages
	  %niri-packages
	  %graphics-packages))
