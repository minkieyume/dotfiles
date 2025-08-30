;;Org配置
;;OrgRoam
(use-package org
  :straight (:type built-in))
(use-package emacsql)

(use-package ox-hugo
  :after ox)

;; (use-package org-roam
;;   :ensure t
;;   :straight t
;;   :custom
;;   (org-roam-directory (file-truename "~/工作目录/Niko之家"))
;;   ;; :bind (("C-c n l" . org-roam-buffer-toggle)
;;   ;;        ("C-c n f" . org-roam-node-find)
;;   ;;        ("C-c n g" . org-roam-ui-open)
;;   ;;        ("C-c n i" . org-roam-node-insert)
;;   ;;        ("C-c n c" . org-roam-capture)
;;   ;; 	 ("C-c n s" . org-roam-ui-mode)
;;   ;; 	 ("C-c n t" . org-roam-tag-add))
;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   ;; If using org-roam-protocol
;;   (require 'org-roam-protocol))

(setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")

;; (use-package org-roam-ui
;;   :after org-roam
;;   :ensure t
;;   :straight t
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;; (use-package org-ql
;;   :ensure t
;;   :straight t
;;   :bind(("C-c q" . org-ql-search)
;; 	("C-c v" . org-ql-views)))

(use-package org-download
  :config
  (setq org-download-image-dir "./org-assets")
  (add-hook 'org-mode-hook 'org-download-enable))

;; 启用org-mode的自动显示图片以及图片缩放
;; (setq org-startup-with-inline-images t)
;; (setq org-image-actual-width nil)


;; 启用 org-mode 的 LaTeX 支持
;; (use-package xenops
;;   :ensure t
;;   :straight t
;;   :config
;;   (setq xenops-reveal-lazy t) ;; 延迟加载预览
;;   (setq xenops-elements-lazy t) ;; 可见范围内的公式延迟加载
;;   (setq xenops-math-image-scale-factor 0.5) ;; 调整比例，默认值为 1.0
;;   :bind(:map xenops-mode
;; 	     ("C-c r" . xenops-reveal-at-point)
;; 	     ("C-c e" . xenops-render))
;;   :hook ((org-mode . my-enable-xenops-mode)
;; 	 (latex-mode . xenops-mode)
;;          (LaTeX-mode . xenops-mode)))

;; 启用org-journal
;; (use-package org-journal
;;   :ensure t
;;   :straight t
;;   :defer t
;;   :init
;;   (setq org-journal-prefix-key "C-c j ")
;;   :config
;;   (setq org-journal-dir "~/工作目录/小梦之家/小梦日记/"
;; 	org-journal-file-type 'daily
;; 	org-journal-file-format "%Y-%m-%d.org")
;;   (setq org-journal-enable-agenda-integration t))

(defun my-enable-xenops-mode ()
  "Enable xenops-mode, except in org-journal-mode."
  (unless (derived-mode-p 'org-journal-mode)
    (xenops-mode)))

(defun xenops-clear-cache ()
  "清空 Xenops 渲染缓存目录."
  (interactive)
  (let ((cache-dir (expand-file-name "~/.emacs.d/xenops/cache")))
    (when (file-directory-p cache-dir)
      (delete-directory cache-dir t t)
      (message "Xenops 缓存已清空！"))))

;;org-ui
;;自动换行
(add-hook 'org-mode-hook #'visual-line-mode)
;;为窗口添加边界
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

;; (setq
;;  ;; Edit settings
;;  org-auto-align-tags nil
;;  org-tags-column 0n
;;  org-catch-invisible-edits 'show-and-error
;;  org-special-ctrl-a/e t
;;  org-insert-heading-respect-content t

;;  ;; Org styling, hide markup etc.
;;  org-hide-emphasis-markers t
;;  org-pretty-entities t

;;  ;; Agenda styling
;;  org-agenda-tags-column 0
;;  org-agenda-block-separator ?─
;;  org-agenda-time-grid
;;  '((daily today require-timed)
;;    (800 1000 1200 1400 1600 1800 2000)
;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;  org-agenda-current-time-string
;;  "◀── now ─────────────────────────────────────────────────")

;; ;; Ellipsis styling
;; (setq org-ellipsis "…")
;; (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

;;KeyBind
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c C-l") #'org-insert-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c y") 'open-my-org-file)

;;Org Capture
(defun org-journal-find-location ()
;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (my-org-filetags-exist-p)
    (save-excursion
      (goto-char (point-min))
      (insert "#+FILETAGS: 日记 个人\n")))
(unless (eq org-journal-file-type 'daily)
(org-narrow-to-subtree))
(goto-char (point-max)))

(defun my-org-filetags-exist-p ()
  "检查当前文件是否已经包含 FILETAGS 定义。"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^#\\+FILETAGS:" nil t)))

(setq org-refile-use-outline-path 'file) ;; 显示文件名和路径
(setq org-outline-path-complete-in-steps nil) ;; 一次性展示完整路径

;; (setq org-capture-templates '(("j" "写日记" plain (function org-journal-find-location)
;; 			       "** %^{Title} \n%T\n%?" :jump-to-captured t :immediate-finish t)
;; 			      ("i" "灵感记录" entry (file+headline "~/工作目录/小梦之家/雏翼灵感库/雏翼灵感库.org" "雏翼灵感库")
;; 			       "* %^{Title} :灵感记录:\n%U\n%?")
;; 			      ("w" "错题本" entry (file+headline "~/工作目录/小梦之家/学习日常/错题本/错题本.org" "错题本")
;; 			       "* %^{Title} :错题本:\n%U\n%?")
;; 			      ("e" "绘画笔记" entry (file+headline "~/工作目录/小梦之家/绘画笔记/绘画笔记.org")
;; 			       "** %^{Title} \n%U\n%?")
;; 			      ("t" "小梦计划" entry (file "~/工作目录/小梦之家/随便记录/随手计划.org")
;; 			       "* TODO %?\nCaptured \n%U\n%?")
;; 			      ("n" "小梦笔记" entry (file "~/工作目录/小梦之家/随便记录/随手笔记.org")
;; 			       "** Note \n%U\n%?")
;; 			      ("r" "阅读笔记" entry (file "~/工作目录/小梦之家/阅读笔记/待整理笔记.org")
;; 			       "* %^{Title} \n%U\n%?")
;; 			      ("t" "作品分析" entry (file "~/工作目录/小梦之家/作品分析/作品分析草稿.org")
;; 			       "* %^{Title} \n%U\n%?")))

;;Org Agenda
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-start-with-time-grid t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 'day)

;;---------------------------------------------
;;org-agenda-time-grid
;;--------------------------------------------
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


(setq org-agenda-files '("~/工作目录/小梦之家/随便记录/"
			 "~/工作目录/小梦之家/学习日常/学习计划.org"
			 "~/工作目录/小梦之家/生活日常/日常计划.org"))

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

	("i" "雏翼灵感库"
	 tags "灵感记录"
         ((org-agenda-files '("~/工作目录/小梦之家/雏翼灵感库/"))
	  (org-agenda-use-tag-inheritance t)))

	("j" "小梦日记"
	 tags "日记"
         ((org-agenda-files '("~/工作目录/小梦之家/小梦日记/"))
	  (org-agenda-use-tag-inheritance t)))

	("f" "短篇小说"
	 tags "短篇小说"
         ((org-agenda-files '("~/工作目录/小梦之家/故事创作/短篇小说/"))
	  (org-agenda-use-tag-inheritance t)))

	("n" "长篇小说"
	 tags "长篇小说"
         ((org-agenda-files '("~/工作目录/小梦之家/故事创作/长篇小说/"))
	  (org-agenda-use-tag-inheritance t)))
	
	("e" "绘画笔记"
	 tags "绘画笔记"
         ((org-agenda-files '("~/工作目录/小梦之家/绘画笔记/"))
	  (org-agenda-use-tag-inheritance t)))
	
	("r" "阅读笔记"
	 tags "阅读笔记"
         ((org-agenda-files '("~/工作目录/小梦之家/阅读笔记/"))
	  (org-agenda-use-tag-inheritance t)))
	
	("t" "作品分析"
	 tags "作品分析"
         ((org-agenda-files '("~/工作目录/小梦之家/作品分析/"))
	  (org-agenda-use-tag-inheritance t)))
	
	("x" "项目进度"
	 alltodo ""
         ((org-agenda-files '("~/LocalWork/游戏创作/夜之城传说：怪盗莺猫传/游戏档案/项目进度.org"
			      "~/LocalWork/游戏创作/夜之城传说：怪盗莺猫传/程序档案/todo.org")))
	 (org-agenda-use-tag-inheritance t))
	
	 ("p" "程序进度"
	  alltodo ""
          ((org-agenda-files '("~/LocalWork/程序开发/LiquidNeko/features.org")))
	  (org-agenda-use-tag-inheritance t))))

;; ;;时间配置
;; (setq calendar-latitude 40.0024) ;;lat, flat
;; (setq calendar-longitude 116.2962) ;;long是经度

;; ;;Sunrise and Sunset
;; ;;日出而作, 日落而息
;; (defun diary-sunrise ()
;;   (let ((dss (diary-sunrise-sunset)))
;;     (with-temp-buffer
;;       (insert dss)
;;       (goto-char (point-min))
;;       (while (re-search-forward " ([^)]*)" nil t)
;;         (replace-match "" nil nil))
;;       (goto-char (point-min))
;;       (search-forward ",")
;;       (buffer-substring (point-min) (match-beginning 0)))))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (python . t)
;;    (shell . t)
;;    (scheme . t)
;;    (C . t)
;;    (js . t)
;;    (plantuml . t)))

;; (defun diary-sunset ()
;;   (let ((dss (diary-sunrise-sunset))
;;         start end)
;;     (with-temp-buffer
;;       (insert dss)
;;       (goto-char (point-min))
;;       (while (re-search-forward " ([^)]*)" nil t)
;;         (replace-match "" nil nil))
;;       (goto-char (point-min))
;;       (search-forward ", ")
;;       (setq start (match-end 0))
;;       (search-forward " at")
;;       (setq end (match-beginning 0))
;;       (goto-char start)
;;       (capitalize-word 1)
;;       (buffer-substring start end))))

;;自定义函数
(defun org-archive-done-tasks ()
  ;;自动归档
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'agenda))

(defun add-file-tags-to-org-files (directory tag)
  "批量为 DIRECTORY 下的所有 .org 文件添加文件标签 TAG。
如果文件中没有 `#+FILETAGS:`，则自动创建。
如果已有标签，则将 TAG 添加到现有标签中而不覆盖。"
  (interactive "D选择文件夹: \ns输入要添加的标签: ")
  ;; 获取目录下的所有 .org 文件
  (let ((org-files (directory-files-recursively directory "\\.org$")))
    (dolist (file org-files)
      (with-temp-buffer
        ;; 打开文件
        (insert-file-contents file)
        ;; 检查是否已有 FILETAGS
        (goto-char (point-min))
        (if (re-search-forward "^#\\+FILETAGS: " nil t)
            (let ((existing-tags (match-string 0)))
              ;; 如果已有标签，添加新的标签
              (unless (string-match-p (regexp-quote tag) existing-tags)
                (goto-char (match-end 0))
                (insert (concat ":" tag))))
          ;; 如果没有 FILETAGS，插入新标签
          (goto-char (point-min))
          (insert (format "#+FILETAGS: :%s:\n" tag)))
        ;; 保存文件
        (write-region (point-min) (point-max) file)))))

(defun my-get-file-tags (org-file)
  "从 ORG-FILE 中获取文件级标签（#+FILETAGS）。"
  (with-temp-buffer
    (insert-file-contents org-file)
    (goto-char (point-min))
    (let (tags)
      ;; 使用正则搜索 #+FILETAGS: 后的标签内容
      (when (re-search-forward "^#\\+FILETAGS:[ \t]*\\(.*\\)$" nil t)
        ;; 将标签按 `:` 分割，去掉空字符串
        (setq tags (split-string (match-string 1) ":" t "[ \t\n]+")))
      tags)))

(defun my-filter-file-tags (tags)
  "过滤标签列表 TAGS：
1. 去除纯空格或空的标签
2. 去除不含任何中文字符的标签
   （这里用到 [[:multibyte:]] 简单检测非 ASCII 字符，你也可以改成更精确的中文判断，比如 [\u4e00-\u9fa5]）"
  (let ((filtered '()))
    (dolist (tag tags)
      (let ((trimmed (string-trim tag)))
        (unless (or (string-empty-p trimmed)
                    ;; 纯空格标签
                    (string-match-p "^[[:space:]]*$" trimmed)
                    ;; 只保留含有中文或多字节字符的标签
                    (not (string-match-p "[[:multibyte:]]" trimmed)))
          (push trimmed filtered))))
    (nreverse filtered)))

(defun merge-org-files-from-directory (dir output-file)
  "将 DIR 中所有 .org 文件的内容合并到 OUTPUT-FILE 中。
文件名作为一级标题，文件级标签作为标题标签（仅保留含有中文字符的标签）。"
  (interactive "D选择要处理的目录: \nF输出到文件: ")
  (let ((org-files (directory-files dir t "\\.org$")))
    ;; 在输出文件中写入
    (with-temp-file output-file
      ;; 循环所有 org 文件
      (dolist (org-file org-files)
        (let* ((file-name (file-name-sans-extension
                           (file-name-nondirectory org-file)))
               (file-tags (my-get-file-tags org-file))
               ;; 过滤出需要的中文标签
               (valid-tags (my-filter-file-tags file-tags)))
          ;; 插入标题
          ;; 如果有有效标签，则按 :tag1:tag2:... 的方式附加在标题末尾
          (if valid-tags
              (insert (format "* %s :%s:\n\n"
                              file-name
                              (string-join valid-tags ":")))
            (insert (format "* %s\n\n" file-name)))
          ;; 插入文件内容
          (insert (with-temp-buffer
                    (insert-file-contents org-file)
                    (buffer-string)))
          (insert "\n\n"))))))

(defun remove-filetags-lines (file)
  "移除单个 org 文件 FILE 中所有以 #+FILETAGS: 开头的行。"
  (interactive "f选择要处理的 org 文件: ")
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "^#\\+FILETAGS:.*$" nil t)
      (replace-match ""))  ;; 用空字符串替换掉匹配到的行
    (write-region (point-min) (point-max) file))
  (message "已移除 %s 中的所有 #+FILETAGS 行" file))

(defun remove-filetags-lines-from-directory (dir)
  "移除目录 DIR 下所有 .org 文件中的 #+FILETAGS 行。"
  (interactive "D选择要处理的目录: ")
  (dolist (org-file (directory-files dir t "\\.org$"))
    (remove-filetags-lines org-file))
  (message "已完成移除目录 %s 下所有 .org 文件中的 #+FILETAGS 行" dir))

(defun open-my-org-file ()
  "Open my specific Org file."
  (interactive)
  (find-file "~/工作目录/小梦之家/小梦之家中心页.org"))

(provide 'init-org)


