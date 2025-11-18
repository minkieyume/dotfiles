;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules packages develop)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%usually-dev-utils
	    %tree-sitter))

(define %usually-dev-utils
  (specifications->packages
   '("git"
     "scons"
     "make"
     "zig"
     "zig-zls"
     "python"
     "pipx"
     "sbcl"
     "racket"
     "r-s7"
     "clojure"
     "s7"
     "chicken"
     "guile")))

(define %tree-sitter
  (specifications->packages
   '("tree-sitter"
     "tree-sitter-rust"
     "tree-sitter-ron"
     "tree-sitter-zig"
     "tree-sitter-c"
     "tree-sitter-json"
     "tree-sitter-bash"
     "tree-sitter-org"
     "tree-sitter-markdown"
     "tree-sitter-make"
     "tree-sitter-cmake"
     "tree-sitter-clojure"
     "tree-sitter-xml"
     "tree-sitter-ini"
     "tree-sitter-html"
     "tree-sitter-kotlin"
     "tree-sitter-gitignore"
     "tree-sitter-dockerfile"
     "tree-sitter-yaml"
     "tree-sitter-udev"
     "tree-sitter-scheme"
     "tree-sitter-python"
     "tree-sitter-python-manifest"
     "tree-sitter-racket"
     "tree-sitter-plantuml")))
