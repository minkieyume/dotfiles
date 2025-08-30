(setq user-mail-address "minkieyume@163.com"
      user-full-name "MinkieYume")

(require 'auth-source)
(setq auth-sources '("~/Keys/authinfo.gpg"))

(setq gnus-select-method
      '(nnimap "163"
               (nnimap-address "imap.163.com")
	       (nnimap-inbox "INBOX")
	       (nnimap-expunge t)
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq gnus-default-method gnus-select-method)

(setq gnus-secondary-select-methods nil)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.163.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq mm-text-html-renderer 'shr
      gnus-default-charset 'utf-8
      mm-coding-system-priorities '(utf-8 gbk gb2312))

(with-eval-after-load 'imap
  (defun my/imap-id-after-connect (proc)
    "在连接完成后，立即向网易发送 IMAP ID 命令。"
    (when (and (processp proc)
               (string-match "imap\\.163\\.com" (process-name proc)))
      (message "[advice] 对 imap.163.com 发送 ID 命令")
      (imap-send-command proc
                         "ID (\"name\" \"Gnus\" \"version\" \"5.13\" \"vendor\" \"FSF\" \"support-email\" \"info@fsf.org\")")))
  (advice-add 'imap-open :after #'my/imap-id-after-connect))

;;Debug
(setq smtpmail-debug-info t)
(setq smtpmail-debug-verb t)
(setq nnimap-record-commands t)
(setq nnimap-debug t)
(setq gnus-verbose 10)


(provide 'init-email)
