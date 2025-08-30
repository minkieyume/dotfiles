(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-better-default)
(require 'init-packages)
(require 'init-plugins)
(require 'init-dirvish)
(require 'init-org)
(require 'init-email)
(require 'init-cpp)
(require 'init-ai)
(require 'init-keybindings)
(require 'init-custom)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/MinkieYume/工作目录/小梦之家/随便记录/随手笔记.org"
     "/home/MinkieYume/工作目录/小梦之家/随便记录/随手计划.org"
     "/home/MinkieYume/工作目录/小梦之家/学习日常/学习计划.org"
     "/home/MinkieYume/工作目录/小梦之家/生活日常/日常计划.org"))
 '(safe-local-variable-directories '("/home/MinkieYume/LocalWork/Program/Rosenthal/")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
