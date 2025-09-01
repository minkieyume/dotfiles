(add-to-list 'load-path "~/emacs-lisp/")  
(load-file (expand-file-name ".init-themes.el" user-emacs-directory))
(require 'init-theme)

;;更好的默认配置
(global-display-line-numbers-mode 1)
(icomplete-mode 1)
(global-auto-revert-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-truncate-lines 1)

(setq inhibit-startup-screen t)  ; 禁用启动画面
(setq inhibit-startup-message t) ; 禁用启动消息
(setq initial-scratch-message "") ; 可选：清空 *scratch* 缓冲区的初始内容

(setq ring-bell-function 'ignore)
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "emacs/backups/"
				   (or (getenv "XDG_CACHE_HOME")
                                       "~/.cache"))))) ;更改自动保存目录
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
(use-package company
  :bind (:map company-active-map
              ("C-n" . 'company-select-next)
              ("C-p" . 'company-select-previous))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (global-company-mode t))
(use-package vertico
  :init
  (vertico-mode t))

(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package marginalia
  :init
  (marginalia-mode t))
(use-package consult
  :bind(("C-s" . consult-line)
	("C-c s" . consult-ripgrep)))
(use-package embark
  :hook(embark-collect-mode . consult-preview-at-point-mode)
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  :bind(("C-." . embark-act)
	("C-;" . embark-dwim)
	("C-h B" . embark-bindings)
	:map minibuffer-mode-map
	("C-c e" . embark-export)))

;; (use-package embark-consult
;;   :straight t)
(use-package all-the-icons
  :bind("C-c i" . all-the-icons-insert))
(use-package disable-mouse
  :config
  (global-disable-mouse-mode)) ; 全局禁用鼠标
(use-package hydra)
(use-package restart-emacs)
(use-package which-key
  :config
  (which-key-mode))
(global-set-key (kbd "C-c h") 'eshell)
(defun delete-current-file ()
  "Delete the file visited by the current buffer and close the buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (and file (file-exists-p file))
        (when (yes-or-no-p (format "Are you sure you want to delete %s? " file))
          (delete-file file)
          (kill-buffer (current-buffer))
          (message "Deleted file: %s" file))
      (message "No file is associated with this buffer."))))

(global-set-key (kbd "C-c d") 'delete-current-file)
(use-package edit-indirect
  :bind
  (:map org-mode-map
	("C-c e" . edit-indirect-region)))
(use-package yaml-mode)

(use-package conf-mode
  :mode "\\.kdl\\'")
;; (use-package sqlite-mode
;;   :straight t)
;; (use-package ink-mode
;;   :straight t
;;   :mode "\\.ink\\'"
;;   :config
;;   ;; Path to the Inklecate binary, used to playtest
;;   ;; and to check for errors
;;   (setq ink-inklecate-path "/usr/bin/inklecate")

;;   ;; Enable flymake (error reporting)
;;   (add-hook 'ink-mode-hook 'flymake-mode)

;;   ;; Set indentation level
;;   (add-hook 'ink-mode-hook (lambda () (setq tab-width 2))))
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-root-files-bottom-up (cons "Cargo.toml" projectile-project-root-files-bottom-up))
  (setq projectile-project-search-path '(("~/Creator/remote/程序开发/" . 3)
					 ("~/Creator/remote/项目档案/" . 2)
					 ("~/Develop/ProgramDevelop" . 2)
					 ("~/Develop/GameDevelop" . 2)
					 ("~/Develop/conlingue" . 2)))
  (projectile-discover-projects-in-search-path)
  :bind-keymap
  ("C-c p" . projectile-command-map))
;; (use-package llm
;;   :straight (:host github :repo "ahyatt/llm" :branch "main"))
;; (use-package ellama
;;   :straight t
;;   :bind ("C-c e" . ellama)
;;   ;; send last message in chat buffer with C-c C-c
;;   :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
;;   :init
;;   (require 'llm-ollama)
;;   (setopt ellama-language "Chinese")
;;   (setopt ellama-provider
;;   	  (make-llm-ollama
;;   	   ;; this model should be pulled to use it
;;   	   ;; value should be the same as you print in terminal during pull
;;   	   :chat-model "deepseek-r1:8b"
;;   	   :embedding-model "bge-m3:latest"
;;   	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
;;   (setopt ellama-summarization-provider ellama-provider)
;;   (setopt ellama-coding-provider ellama-provider)
  
;;   (setopt ellama-extraction-provider ellama-provider)
;;   ;; Naming Provider
;;   (setopt ellama-naming-provider ellama-provider)
;;   (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
;;   ;; Translater Provider
;;   (setopt ellama-translation-provider ellama-provider)
;;   (setopt ellama-extraction-provider ellama-provider)
;;   (setopt ellama-providers
;;   	  '(("deepseek-r1" . (make-llm-ollama
;;   			      :chat-model "deepseek-r1:8b"
;;   			      :embedding-model "bge-m3:latest"
;; 			      :default-chat-non-standard-params '(("num_ctx" . 8192))))))
;;   :config
;;   (setopt ellama-auto-scroll t)
;;   ;; show ellama context in header line in all buffers
;;   (ellama-context-header-line-global-mode +1)
;;   ;; show ellama session id in header line in all buffers
;;   (ellama-session-header-line-global-mode +1)
;;   (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
;;   (advice-add 'end-of-buffer :after #'ellama-enable-scroll))
;; (use-package aider
;;   :straight (:host github :repo "tninja/aider.el")
;;   :bind (("C-c C-a" . aider-transient-menu))
;;   :custom
;;   (aider-popular-models '("ollama_chat/starcoder2:instruct" "ollama_chat/deepseek-coder-v2:16b-lite-instruct-q4_K_M"))
;;   :config
;;   (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434"))
;; (use-package triples
;;   :straight t)
;; (use-package ekg
;;   :straight (:host github :repo "MinkieYume/ekg" :branch "develop")
;;   :bind (("C-c n c" . ekg-capture)
;; 	 ("C-c n u" . ekg-capture-url)
;; 	 ("C-c n f" . ekg-capture-file)
;; 	 ("C-c n s" . ekg-search)
;; 	 ("C-c n S" . ekg-embedding-search)
;; 	 ("C-c n q" . ekg-llm-query-with-notes)
;; 	 ("C-c n D" . ekg-show-notes-in-drafts)
;; 	 ("C-c n T" . ekg-show-notes-for-trash)
;; 	 ("C-c n o" . ekg-browse-url)
;; 	 ("C-c n d" . ekg-show-notes-for-today)
;; 	 ("C-c n t" . ekg-show-notes-with-tag)
;; 	 ("C-c n w" . ekg-llm-send-and-append-note)
;; 	 ("C-c n r" . ekg-llm-send-and-replace-note)
;; 	 ("C-c n L" . ekg-show-notes-latest-captured)
;; 	 ("C-c n l" . ekg-show-notes-latest-modified))
;;   :init
;;   (require 'ekg-embedding)
;;   (ekg-embedding-generate-on-save)
;;   (require 'ekg-llm)
;;   (require 'llm-ollama)  ;; The specific provider you are using must be loaded.
;;   (let ((deepseek-r1 (make-llm-ollama
;; 		      :chat-model "deepseek-r1:8b"
;; 		      :embedding-model "bge-m3:latest"
;; 		      :default-chat-non-standard-params '(("num_ctx" . 8192))))
;; 	(phi4 (make-llm-ollama
;; 	       :chat-model "phi4-mini:latest"
;; 	       :embedding-model "bge-m3:latest"
;; 	       :default-chat-non-standard-params '(("num_ctx" . 8192))))
;; 	(qwen3 (make-llm-ollama
;; 		:chat-model "qwen3:4b"
;; 		:embedding-model "bge-m3:latest"
;; 		:default-chat-non-standard-params '(("num_ctx" . 8192))))
;; 	(bge-m3 (make-llm-ollama
;; 		 :embedding-model "bge-m3:latest")))
;;     (setq ekg-llm-provider qwen3
;;           ekg-embedding-provider bge-m3))
;;   :config
;;   (setq ekg-db-file "~/Creator/remote/YumiEko/yumieko.db")
;;   (setq warning-suppress-types '((org-element)))
;;   (setq ekg-truncation-method 'character)
;;   :custom
;;   (require 'ekg-logseq)
;;   (setq ekg-logseq-dir "~/Creator/remote/YumiEko/logseq/")
;;   (ekg-logseq-export))
(use-package org)
(use-package emacsql)
(use-package ox-hugo
  :after ox)
(setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c C-l") #'org-insert-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(use-package org-download
  :config
  (setq org-download-image-dir "./org-assets")
  (add-hook 'org-mode-hook 'org-download-enable))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-start-with-time-grid t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 'day)
(setq agenda-use-time-grid t)
(setq org-agenda-time-grid '((daily today require-timed)
                                   (300
                                    600
                                    900
                                    1200
                                    1500
                                    1800
                                    2100
                                    2400)
                                   "......"
                                   "-----------------------------------------------------"
                                   ))
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "DAILY(l)" "|" "DONE(d)" "CANCELED(c)")))
(setq org-agenda-files '("~/Creator/remote/小梦之家/随便记录/"
			 "~/Creator/remote/小梦之家/学习日常/学习计划.org"
			 "~/Creator/remote/小梦之家/生活日常/日常计划.org"))
(setq org-agenda-custom-commands
      '(("c" "日程安排界面"
	 ((tags "PRIORITY=\"A\""
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "优先处理")))
          (agenda "")
          (tags "REFILE"
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		 (org-agenda-overriding-header "待整理日程")
                 (org-tags-match-list-sublevels nil)))
	  (alltodo ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'scheduled))
		    (org-tags-match-list-sublevels t)
                    (org-agenda-overriding-header "未计划事项")))))  	
	("x" "项目进度"
	 alltodo ""
	 ((org-agenda-files '("~/LocalWork/游戏创作/夜之城传说：怪盗莺猫传/游戏档案/项目进度.org"
			      "~/LocalWork/游戏创作/夜之城传说：怪盗莺猫传/程序档案/todo.org")))
	 (org-agenda-use-tag-inheritance t))
	
	("p" "程序进度"
	 alltodo ""
         ((org-agenda-files '("~/LocalWork/程序开发/LiquidNeko/features.org")))
	 (org-agenda-use-tag-inheritance t))))
(use-package cmake-mode)

;; (use-package cmake-ide
;;   :straight t
;;   :bind (:map c-mode-map
;; 	      ("C-c C-r" . cmake-ide-compile))
;;   :config
;;   (cmake-ide-setup))
;; 基本语法高亮 & 缩进支持
(add-hook 'c++-mode-hook #'electric-pair-mode)
(add-hook 'c++-mode-hook #'show-paren-mode)
(add-hook 'c++-mode-hook #'display-line-numbers-mode)
(add-hook 'c++-mode-hook #'indent-tabs-mode)

;; 设置基本缩进宽度
(setq-default c-basic-offset 4)

;; 启动 GDB 时开启多窗口布局
(setq gdb-many-windows t
      gdb-show-main t)

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(use-package gdscript-mode
  :hook (gdscript-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(gdscript-mode . gdscript-ts-mode))
  :config
  (setq gdscript-godot-executable "~/Applications/godot.x86_64")
  (setq gdscript-eglot-version 4.4))
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-mode-treesitter-derive t)
  :hook((rust-mode . eglot-ensure)
	(rust-mode . (lambda () (setq indent-tabs-mode nil)))
	(rust-mode . (lambda () (prettify-symbols-mode)))))
;; (use-package cargo
;;   :straight t
;;   :hook(rust-mode . cargo-minor-mode)
;;   :config
;;   (define-key cargo-minor-mode-command-map (kbd "C-r") #'cargo-run-eshell))
(defun cargo-run-eshell ()
  "在另一个窗口智能打开 *cargo-eshell*，并运行 cargo run。"
  (interactive)
  (let* ((buf-name "*cargo-eshell*")
	 (default-directory (file-name-directory (or buffer-file-name default-directory)))
	 (buf (or (get-buffer buf-name)
                  (save-window-excursion
                    (let ((b (eshell "new")))
                      (with-current-buffer b
			(rename-buffer buf-name))
                      b)))))
    (display-buffer buf
                    '((display-buffer-reuse-window
		       display-buffer-use-some-window
                       display-buffer-pop-up-window)
		      (inhibit-same-window . t)))
    (with-current-buffer buf
      (goto-char (point-max))
      (eshell-send-eof-to-process)
      (insert "cargo run")
      (eshell-send-input))))
(use-package scheme-mode
  :mode "\\.neko\\'")

(use-package racket-mode)
(use-package geiser
  :config
  (setq geiser-active-implementations '(guile chibi racket)
	geiser-mode-auto-p nil))

(use-package geiser-guile)

;; (use-package geiser-chibi
;;   :straight t)
(use-package rainbow-delimiters
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))
(use-package paredit
  :hook((emacs-lisp-mode lisp-mode scheme-mode racket-mode racket-repl-mode) . enable-paredit-mode))
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))
(use-package treesit
  :config (setq treesit-font-lock-level 4)
  :init
  (setq treesit-extra-load-path '("~/.treesitter/gdscript")))
(use-package magit
  :config
  (with-eval-after-load 'git-commit
    (setq git-commit-cd-to-toplevel t)))
(use-package pinentry
  :config
  (pinentry-start)
  :custom
  (epa-pinentry-mode 'loopback))
(use-package pdf-tools)
(use-package ement)
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  ;:hook
  ;(dirvish-setup . dirvish-emerge-mode)
  :custom
  ;快速访问
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads"                  "下载")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")
     ("D"  "~/Develop"                  "开发目录")
     ("C"  "~/Creator"                  "创作目录")
     ("P"  "~/Pictures"                 "图片目录")
     ("g"  "~/Develop/GameDevelop"      "游戏开发")
     ("p"  "~/Develop/ProgramDevelop"   "程序开发")
     ("c"  "~/Creator/conlingue"        "造语")
     ("s"  "~/Creator/conlingue/shangrira" "香格里拉语")
     ("e"  "~/Creator/remote/绘画创作"  "绘画")
     ("M"  "~/Creator/remote/音乐创作"  "音乐")
     ("R"  "~/Creator/remote"           "远程创作")))
  ;分类组
  ;; (dirvish-emerge-groups
  ;;  '(("最近文件" (predicate . recent-files-2h))
  ;;    ("文档" (extensions "pdf" "tex" "bib" "equb"
  ;; 			   "org" "txt" "md"))
  ;;    ("视频" (extensions "mp4" "mkv" "webm"))
  ;;    ("音频" (extensions "mp3" "flac" "wav" "ape" "aac"
  ;; 			   "tak" "midi"))
  ;;    ("压缩包" (extensions "gz" "rar" "zip"))))

  ;; Dirvish程序绑定
   (dirvish-fd-program "$$bin/fd")
   (dirvish-7z-program "$$bin/7z$$")
   (dirvish-vipsthumbnail-program "$$bin/vipsthumbnail$$")
   (dirvish-ffmpegthumbnailer-program "$$bin/ffmpegthumbnailer$$")
   (dirvish-mediainfo-program "$$bin/mediainfo$$")
  ;;(dirvish-magick-program "$$bin/magick$$")     

  ;; Dirvish功能配置
  (dirvish-large-directory-threshold 100)
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (delete-by-moving-to-trash t)
  (dired-listing-switches
   "-lhv --group-directories-first")
  (dirvish-default-layout '(0 0.11 0.55))    
  :config
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access) ;快速访问
   ("f"   . dirvish-file-info-menu) ;文件信息
   ("y"   . dirvish-yank-menu) ;剪贴板菜单
   ("N"   . dirvish-narrow) ;过滤文件列表
   ("^"   . dirvish-history-last) ;上一条历史
   ("h"   . dirvish-history-jump) ; remapped `describe-mode' 历史跳转
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit' 快速排序
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file' 版本控制
   ("w" . dirvish-copy-file-path)
   ("TAB" . dirvish-subtree-toggle) ;子树切换
   ("M-f" . dirvish-history-go-forward) ;导航到下一个历史位置
   ("M-b" . dirvish-history-go-backward) ;导航到上一个历史位置
   ("M-l" . dirvish-ls-switches-menu) ;设置ls选项
   ("M-m" . dirvish-mark-menu) ;标记操作菜单
   ("M-t" . dirvish-layout-toggle) ;布局切换
   ("M-s" . dirvish-setup-menu) ;设置菜单
   ("M-e" . dirvish-emerge-menu) ;文件归类菜单
   ("M-j" . dirvish-fd-jump) ;搜索跳转
   ("M-u" . dirvish-jump-up)))
(use-package dired-git-info)
(defun clear-trash ()
  (interactive)
  (let ((trash-dirs
	 (list "~/.local/share/Trash/files" "~/.local/share/Trash/info" "~/.Trash")))
    (when (yes-or-no-p "确定要清空回收站吗？此操作不可撤销。")
      (dolist (dir trash-dirs)
        (when (file-directory-p (expand-file-name dir))
          (delete-directory (expand-file-name dir) t t)
          (make-directory (expand-file-name dir))))
      (message "回收站已清空。"))))

(defun dirvish-jump-up ()
  "跳转到当前目录的上一级目录，等价于使用 `..` 进入上级目录。"
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((parent-dir (file-name-directory (directory-file-name default-directory))))
        (if parent-dir
            (dired parent-dir)
          (message "当前目录没有上一级目录！")))
    (message "当前缓冲区不是 Dirvish 或 Dired 模式。")))
(use-package eat
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))
(use-package eshell-syncthing-highlighting
  :after eshell-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))
(when (and (executable-find "$$bin/fish$$")
                 (require 'fish-completion nil t))
        (global-fish-completion-mode))
