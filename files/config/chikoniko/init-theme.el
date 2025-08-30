(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-dark t))
;(use-package zenburn-theme
;  :ensure t
;  :config
;  (load-theme 'zenburn t))

(set-face-attribute 'default nil
		    :family "Sarasa Term SC"
		    :height 120)

(provide 'init-theme)
