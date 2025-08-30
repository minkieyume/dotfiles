(require 'server)
;; 环境变量
(add-to-list 'exec-path "~/.local/bin")

;;更好的默认配置
(global-display-line-numbers-mode 1)
(icomplete-mode 1)
(global-auto-revert-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-truncate-lines 1)

;(setq debug-on-error t) ; 错误时显示调试信息
(setq inhibit-startup-screen t)  ; 禁用启动画面
(setq inhibit-startup-message t) ; 禁用启动消息
(setq initial-scratch-message "") ; 可选：清空 *scratch* 缓冲区的初始内容

(setq ring-bell-function 'ignore)
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/"))) ;更改自动保存目录
(unless (server-running-p) (server-start));启用服务器

;; ;; Scheme开发
;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode nil)
;;             (setq tab-width 2)
;;             (setq lisp-indent-offset 2)))  ;; 影响大多数 Lisp/Scheme 方言

(provide 'init-better-default)
