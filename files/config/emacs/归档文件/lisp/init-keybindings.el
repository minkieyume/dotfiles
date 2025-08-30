;按键绑定
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-c h") 'eshell)
;; 文件末尾
(provide 'init-keybindings)
