(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; 自定义快捷访问，setq没用。
   '(("h" "~/"                          "Home")
     ("d" "~/下载"                      "下载")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")
     ("e" "~/.emacs.d/lisp/"            "Emacs Lisp")
     ("E" "~/.emacs.d/"                 "Emacs")
     ("w" "~/工作目录/"                 "工作目录")
     ("Y" "~/工作目录/小梦之家/"        "小梦之家")
     ("y" "~/yumieko/"                  "Yumieko")
     ("G" "~/Games/"                    "游戏")
     ("g" "~/LocalWork/游戏创作/"        "游戏项目")
     ("c" "~/LocalWork/造语/香格里拉文/" "造语项目")
     ("b" "~/yumieko/content-org/" "博文编写")
     ("s" "~/工作目录/游戏开发/素材库/"        "素材库")
     ("C" "~/工作目录/游戏开发/社团项目/" "社团项目")
     ("M" "/mnt/nfs/music" "音乐")
     ("l" "~/LocalWork/" "本地工作")))
  :config
  (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "--human-readable --group-directories-first --no-group")
  (setq dirvish-default-layout '(0 0.11 0.55))
  
   :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
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
   ("M-e" . dirvish-emerge-menu) ;合并菜单
   ("M-j" . dirvish-fd-jump) ;搜索跳转
   ("M-u" . dirvish-jump-up)))

(use-package dired-git-info
  :ensure t
  :straight t)

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

(provide 'init-dirvish)
