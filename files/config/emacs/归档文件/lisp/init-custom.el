;;自定义配置

;; 禁用自动备份和自动保存
;(setq auto-save-default nil) ;(setq make-backup-files nil)

;;Treesit设置
(use-package treesit
  :config (setq treesit-font-lock-level 4)
  :init
  (setq treesit-extra-load-path '("~/.emacs.d/treesit/tree-sitter-gdscript/src/")))

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

(defun open-kitty-in-current-dir ()
  "在当前 Emacs buffer 的目录中打开 Kitty 终端。"
  (interactive)
  (start-process "kitty-terminal" nil "kitty" "--working-directory" default-directory))

(global-set-key (kbd "C-c k") 'open-kitty-in-current-dir)

(add-to-list 'load-path "~/.emacs.d/lisp/ai_tools")
(require 'org-roam-export-ekg)

(provide 'init-custom)
