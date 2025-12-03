(eval-when-compile (require 'cl-lib))
(require 'cl-lib)
(load-theme 'spacemacs-dark t)

(set-face-attribute 'default nil
		    :family "Sarasa Term SC"
		    :height 120)

;; 向量数据库：chikoyumemi 限定
(use-package vecdb
  :config
  (require 'vecdb-qdrant)
  (defvar chiko-vecdb-provider (make-vecdb-qdrant-provider :api-key (string-trim (with-temp-buffer
										   (insert-file-contents "$$qdrant-api-key.txt$$")
										   (buffer-string)))
  							 :url "http://chikoyumemi:6333"))
  (setq ekg-vecdb-provider (cons chiko-vecdb-provider (make-vecdb-collection :name "ekg" :vector-size 1024))))

(provide 'init-theme)
