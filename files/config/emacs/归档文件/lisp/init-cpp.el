;;C++环境配置

(use-package cmake-mode)

(use-package cmake-ide
  :straight t
  :bind (:map c-mode-map
	      ("C-c C-r" . cmake-ide-compile))
  :config
  (cmake-ide-setup))


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

(provide 'init-cpp)
