;;插件配置

;; 内建包引用
;(require 'emacsql)

;;补全优化
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

(use-package consult
  :bind(("C-s" . consult-line)
	("C-c s" . consult-ripgrep)))

(use-package marginalia
  :init
  (marginalia-mode t))

(use-package embark
  :hook(embark-collect-mode . consult-preview-at-point-mode)
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  :bind(("C-." . embark-act)
	("C-;" . embark-dwim)
	("C-h B" . embark-bindings)
	:map minibuffer-mode-map
	("C-c e" . embark-export)))

(use-package embark-consult
  :straight t)

;;字体图标
(use-package all-the-icons
  :bind("C-c i" . all-the-icons-insert))

(use-package magit
  :config
  (with-eval-after-load 'git-commit
  (setq git-commit-cd-to-toplevel t)))

;;模式配置
(use-package gdscript-mode
    :hook (gdscript-mode . eglot-ensure)
    :init
    (add-to-list 'major-mode-remap-alist '(gdscript-mode . gdscript-ts-mode))
    :config
    (setq gdscript-godot-executable "~/Applications/godot.x86_64")
    (setq gdscript-eglot-version 4.4))

(use-package yaml-mode)

(use-package sqlite-mode
  :straight t)

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
;  (setq rust-mode-treesitter-derive t)
  :hook((rust-mode . eglot-ensure)
	(rust-mode . (lambda () (setq indent-tabs-mode nil)))
	(rust-mode . (lambda () (prettify-symbols-mode)))))

(use-package cargo
  :straight t
  :hook(rust-mode . cargo-minor-mode)
  :config
  (define-key cargo-minor-mode-command-map (kbd "C-r") #'cargo-run-eshell))

;; [rainbow-delimiters] Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5))

(use-package paredit
  :straight t
  :hook((emacs-lisp-mode lisp-mode scheme-mode racket-mode racket-repl-mode) . enable-paredit-mode))

(use-package scheme-mode
  :mode "\\.neko\\'")

(use-package racket-mode)

(use-package geiser
  :config
  (setq geiser-active-implementations '(guile chibi racket)
	geiser-mode-auto-p nil))

(use-package geiser-guile)

(use-package geiser-chibi
  :straight t)

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

(use-package ink-mode
  :straight t
  :mode "\\.ink\\'"
  :config
  ;; Path to the Inklecate binary, used to playtest
  ;; and to check for errors
  (setq ink-inklecate-path "/usr/bin/inklecate")

  ;; Enable flymake (error reporting)
  (add-hook 'ink-mode-hook 'flymake-mode)

  ;; Set indentation level
  (add-hook 'ink-mode-hook (lambda () (setq tab-width 2))))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;;键位插件
(use-package disable-mouse
  :config
  (global-disable-mouse-mode)) ; 全局禁用鼠标
(use-package hydra)

(use-package restart-emacs)
(use-package which-key
  :config
  (which-key-mode))

;;阅读pdf
(use-package pdf-tools)

;;即时通讯
(use-package ement)

;; (use-package sly
;;   :straight t
;;   :bind("C-c (" . sly)
;;   :init
;;   (setq inferior-lisp-program "sbcl")
;;   (setenv "SBCL_HOME" "/usr/lib64/sbcl"))

;;项目管理
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-root-files-bottom-up (cons "Cargo.toml" projectile-project-root-files-bottom-up))
  (setq projectile-project-search-path '(("~/工作目录/程序开发/" . 3)
					 ("~/工作目录/游戏开发/项目档案/" . 2)
					 ("~/LocalWork/程序开发/" . 2)
					 ("~/LocalWork/游戏创作/" . 2)
					 ("~/LocalWork/造语/" . 2)))
  (projectile-discover-projects-in-search-path)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package circe
  :config
  (setq circe-network-options
      '(("Libera"
         :host "irc.libera.chat"
         :port 6667
         :nick "MinkieYume"
	 :sasl-username "MinkieYume"
         :sasl-password "MinkieDash8.1"
	 :tls nil
         :channels ("#emacs-zh" "#gentoo-cn")))))

(defun libera-irc ()
  (interactive)
  (circe "Libera"))



(provide 'init-plugins)
