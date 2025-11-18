;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets ai)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module (chiko services ai)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules sets)
  #:export (make-ollama-nvidia
	    make-qdrant
	    make-aider
	    make-ai-servers
	    make-ai))

(define ollama-gpu-wrapper
  (program-file
   "ollama-gpu-wrapper"
   #~(begin
       (setenv "LD_LIBRARY_PATH"
               (string-append #$(file-append (spec->pkg "nvidia-driver") "/lib")))
       (setenv "OLLAMA_HOST" "0.0.0.0:11434")
       (setenv "OLLAMA_DEBUG" "1")
       (execl #$(file-append (spec->pkg "ollama-linux-amd64-nvidia") "/bin/ollama") "ollama" "serve"))))

(define (make-ollama-nvidia)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("ollama-linux-amd64-nvidia")))))
   (home-settings `((services
		     ,(list (simple-service 'ollama-serve
					    home-shepherd-service-type
					    (list (shepherd-service
						   (provision '(ollama))
						   (documentation "Run ollama serve daemon")
						   (start #~(make-forkexec-constructor
							     (list #$ollama-gpu-wrapper)
							     #:log-file "log/ollama-serve.log"))
						   (stop #~(make-kill-destructor)))))))))))

(define* (make-qdrant #:key (image "qdrant/qdrant:v1.15.5-gpu-nvidia"))
  (cfgset
   (sys-settings `((services
		    ,(list (service qdrant-service-type
				    (qdrant-configuration
				     (image image)
				     (api-key (secret-ref 'qdrant-api-key))))))))))
(define aider-wrapper
  (program-file
   "aider-wrapper"
   #~(begin
       (apply execl (append
		     (list (string-append (getenv "HOME") "/.local/bin/uvx")
			   "uvx" "--from" "aider-chat" "aider")
		     (cdr (command-line)))))))

(define (make-aider)
  (cfgset
   (home-files `((".local/bin/aider" ,aider-wrapper)))))

(define (make-ai-servers)
  (merge-sets
   (make-ollama-nvidia)
   (make-qdrant)))

(define (make-ai)
  (merge-sets
   (make-ai-servers)
   (make-aider)))
