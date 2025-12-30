(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(load-theme 'spacemacs-dark t)

(set-face-attribute 'default nil
		    :family "Sarasa Term SC"
		    :height 120)

(use-package disable-mouse
  :config
  (global-disable-mouse-mode)) ; 全局禁用鼠标

(provide 'init-theme)
