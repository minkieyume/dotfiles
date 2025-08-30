;; AI配置
;; 前置安装
(use-package llm
  :straight (:host github :repo "ahyatt/llm" :branch "main"))
(use-package triples
  :straight t)

;; ellama
(use-package ellama
  :straight t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (require 'llm-ollama)
  (setopt ellama-language "Chinese")
  (setopt ellama-provider
  	  (make-llm-ollama
  	   ;; this model should be pulled to use it
  	   ;; value should be the same as you print in terminal during pull
  	   :chat-model "deepseek-r1:8b"
  	   :embedding-model "bge-m3:latest"
  	   :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider ellama-provider)
  (setopt ellama-coding-provider ellama-provider)
  
  (setopt ellama-extraction-provider ellama-provider)
  ;; Naming Provider
  (setopt ellama-naming-provider ellama-provider)
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translater Provider
  (setopt ellama-translation-provider ellama-provider)
  (setopt ellama-extraction-provider ellama-provider)
  (setopt ellama-providers
  	  '(("deepseek-r1" . (make-llm-ollama
  			 :chat-model "deepseek-r1:8b"
  			 :embedding-model "bge-m3:latest"
			 :default-chat-non-standard-params '(("num_ctx" . 8192))))))
  :config
  (setopt ellama-auto-scroll t)
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1)
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))

(use-package ekg
  :straight (:host github :repo "MinkieYume/ekg" :branch "develop")
  :bind (("C-c n c" . ekg-capture)
	 ("C-c n u" . ekg-capture-url)
	 ("C-c n f" . ekg-capture-file)
	 ("C-c n s" . ekg-search)
	 ("C-c n S" . ekg-embedding-search)
	 ("C-c n q" . ekg-llm-query-with-notes)
	 ("C-c n D" . ekg-show-notes-in-drafts)
	 ("C-c n T" . ekg-show-notes-for-trash)
	 ("C-c n o" . ekg-browse-url)
	 ("C-c n d" . ekg-show-notes-for-today)
	 ("C-c n t" . ekg-show-notes-with-tag)
	 ("C-c n w" . ekg-llm-send-and-append-note)
	 ("C-c n r" . ekg-llm-send-and-replace-note)
	 ("C-c n L" . ekg-show-notes-latest-captured)
	 ("C-c n l" . ekg-show-notes-latest-modified))
  :init
  (require 'ekg-embedding)
  (ekg-embedding-generate-on-save)
  (require 'ekg-llm)
  (require 'llm-ollama)  ;; The specific provider you are using must be loaded.
  (let ((deepseek-r1 (make-llm-ollama
  			 :chat-model "deepseek-r1:8b"
			 :embedding-model "bge-m3:latest"
			 :default-chat-non-standard-params '(("num_ctx" . 8192))))
	(phi4 (make-llm-ollama
  			 :chat-model "phi4-mini:latest"
			 :embedding-model "bge-m3:latest"
			 :default-chat-non-standard-params '(("num_ctx" . 8192))))
	(qwen3 (make-llm-ollama
  			 :chat-model "qwen3:4b"
			 :embedding-model "bge-m3:latest"
			 :default-chat-non-standard-params '(("num_ctx" . 8192))))
	(bge-m3 (make-llm-ollama
		         :embedding-model "bge-m3:latest")))
    (setq ekg-llm-provider qwen3
          ekg-embedding-provider bge-m3))
  :config
  (setq ekg-db-file "~/工作目录/YumiEko/yumieko.db")
  (setq warning-suppress-types '((org-element)))
  (setq ekg-truncation-method 'character)
  :custom
  (require 'ekg-logseq)
  (setq ekg-logseq-dir "~/工作目录/YumiEko/logseq/")
  (ekg-logseq-export))

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :bind (("C-c C-a" . aider-transient-menu))
  :custom
  (aider-popular-models '("ollama_chat/starcoder2:instruct" "ollama_chat/deepseek-coder-v2:16b-lite-instruct-q4_K_M"))
  :config
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434"))

;; (use-package aidermacs
;;   :ensure t
;;   :straight t
;;   :bind (("C-c C-a" . aidermacs-transient-menu))
;;   :config
;;   (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
;;   :custom
;;   ; See the Configuration section below
;;   (aidermacs-use-architect-mode t))

;; (use-package tabby
;;   :straight (tabby
;; 	     :type git
;; 	     :host github
;; 	     :files ("*.el" "node_scripts")
;; 	     :repo "alan-w-255/tabby.el"))

(provide 'init-ai)
