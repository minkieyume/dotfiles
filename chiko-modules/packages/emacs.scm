;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules packages emacs)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%emacs-packages))

(define %emacs-packages
  (specifications->packages
   '("emacs-pgtk"
     "emacs-eldev"

     ;;包管理器
     "emacs-use-package"  

     ;;编辑模式
     "emacs-nginx-mode"
     "emacs-edit-indirect"
     "emacs-fish-mode"
     "emacs-json-mode"
     "emacs-markdown-mode"
     "emacs-nftables-mode"
     "emacs-zig-mode"  
     "emacs-cmake-mode"
     "emacs-gdscript-mode"
     "emacs-yaml-mode"
     "emacs-rust-mode"
     "emacs-rustic"
     "emacs-racket-mode"
     "emacs-geiser"
     "emacs-geiser-guile"
     "emacs-plantuml-mode"
     "emacs-scribble-mode"
     "emacs-inheritenv"
     "emacs-direnv"
     "emacs-envrc"
     "emacs-guix"

     ;;编辑器优化
     "emacs-company"
     "emacs-company-box-chiko"
     "emacs-vertico"
     "emacs-orderless"
     "emacs-consult"
     "emacs-ripgrep"
     "ripgrep"
     "ripgrep-all"
     "emacs-marginalia"
     "emacs-embark"
     "emacs-rainbow-delimiters"
     "emacs-paredit"
     "emacs-smartparens"  

     ;;键位优化
     "emacs-disable-mouse"
     "emacs-hydra"
     "emacs-restart-emacs"
     "emacs-which-key"

     ;;万能工具
     "emacs-pinentry"
     "emacs-pdf-tools"
     "emacs-ement"
     "emacs-projectile"
     "emacs-circe"
     "emacs-emacsql"
     "emacs-ox-hugo"
     "emacs-org-download"
     "emacs-oauth2"
     "emacs-inheritenv"

     ;;AI集成
     "emacs-llm"
     "emacs-vecdb"
     "emacs-copilot"
     "emacs-copilot-chat"
     "emacs-mcp"
     "emacs-ellama"
     ;;"emacs-elisa"
     "emacs-aider"

     ;;笔记软件
     "emacs-ekg"

     ;;终端优化
     "emacs-eat"
     "emacs-eshell-syntax-highlighting"
     "emacs-fish-completion"

     ;;RSS订阅
     "emacs-elfeed"
     "emacs-elfeed-goodies"
     "emacs-elfeed-score"
     "emacs-elfeed-tube"
     "emacs-elfeed-protocol"
     "emacs-elfeed-org"

     ;;版本控制
     "emacs-magit"
     "emacs-magit-todos"
     "emacs-forge"

     ;;文件管理
     "emacs-dirvish@d877433f957a363ad78b228e13a8e5215f2d6593"
     "emacs-dired-git-info"

     ;;主题资源
     "emacs-all-the-icons"
     "emacs-spacemacs-theme"     

     ;;外部依赖
     "plantuml"
     "pandoc")))
