;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets develop)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages develop)
  #:use-module (rustup build toolchain)
  #:export (make-direnv
	    make-c
	    make-rust
	    make-uv
	    make-develop))

(define (make-direnv)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("direnv")))))
   (home-configs `(("fish/conf.d/direnv.fish"
  		  ,(plain-file "direnv.fish" "direnv hook fish | source"))))))

(define (make-c)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("ccls")))))
   (home-files `((".local/bin/cc" ,(symlink-to (file-append (spec->pkg "gcc-toolchain") "/bin/gcc")))))))

(define (make-rust)
  (cfgset
   (sys-settings `((packages
		    ,(list (rustup "stable"
				   #:components
				   (list "cargo" "rustc" "rustfmt" "rust-std"
					 "rust-src" "clippy" "miri" "rust-analyzer"
					 "rust-mingw"))))))
   (home-envs `(("CARGO_REGISTRY_TOKEN" . ,(secret-ref 'crates-io))))))

(define (make-uv) ; See 'Install UV.org'
  (cfgset
   (home-envs `(("PATH" . "$HOME/.local/bin:$PATH")
		("UV_INDEX_URL" . "https://pypi.tuna.tsinghua.edu.cn/simple")))))

(define (make-develop)
  (merge-sets
   (make-direnv)
   (make-c)
   (make-rust)
   (make-uv)
   (cfgset
    (sys-settings `((packages
		     ,(append %usually-dev-utils %common-lisp %scheme %tree-sitter)))))))
