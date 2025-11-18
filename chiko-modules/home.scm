;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules home)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-home
	    make-home*
	    make-home-env
	    make-home-dotfiles
	    make-home-files
	    make-home-config
	    make-home-desktop
	    make-home-mime))

(define %default-config
  `((packages ,(specifications->packages '("hello")))))

;; (define-syntax make-home
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((_ (trans ...) (config ...))
;;        (let* ((cfgs (syntax->datum #'(config ...)))
;; 	      (vcfgs (merge-config %default-config cfgs))
;; 	      (stx-cfgs (datum->syntax stx (filter valid-cfg? vcfgs))))
;; 	 (with-syntax (((config ...) stx-cfgs))
;; 	   #'((compose (lambda (x) x) trans ...)
;; 	      (home-environment
;; 	       config ...))))))))

;; (define-syntax-rule (make-home-env env ...)
;;   (simple-service 'home-environment-variables
;; 		  home-environment-variables-service-type
;; 		  (list env ...)))

;; (define-syntax-rule (make-home-dotfiles dotfiles ...)
;;   (service home-dotfiles-service-type
;; 	   (home-dotfiles-configuration
;;   	  (directories (list dotfiles ...)))))

;; (define-syntax-rule (make-home-files (path file) ...)
;;   (simple-service 'chiko-home-files
;; 		  home-files-service-type
;; 		  `((path file)  ...)))

;; (define-syntax-rule (make-home-config (path file) ...)
;;   (simple-service 'home-xdg-configuration
;;   		home-xdg-configuration-files-service-type
;; 		  `((path file)  ...)))

;; (define-syntax-rule (make-home-desktop (desktop package) ...)
;;   (simple-service 'home-desktop
;;   		home-files-service-type
;;   		`((,(string-append ".local/share/applications/" desktop)
;; 		     ,(computed-substitution-with-inputs (string-append desktop)
;;   						       (local-file (string-append %desktopdir desktop))
;;   						       (list (spec->pkg package)))) ...)))

;; (define-syntax-rule (make-home-mime mime ...)
;;   (simple-service 'default-mimes
;;   		home-xdg-configuration-files-service-type
;;   		`(("mimeapps.list"
;; 		     ,(plain-file "mimeapps.list" (string-join '("[Default Applications]" mime ...) "\n"))))))

;; (define (make-home transforms configs)
;;   (let* ((vcfgs (merge-config %default-config configs))
;;          (filtered-cfgs (filter valid-cfg? vcfgs)))
;;     ((apply compose (cons (lambda (x) x) transforms))
;;      (apply home-environment filtered-cfgs))))

;; (define (make-home set)
;;   (let* ((home-expr `(home-environment
;; 		      ,@(merge-config %default-config
;; 				      `((services ,(apply make-home-env (cfgset-home-envs set))
;; 						  ,(make-home-dotfiles %dotfilesdir)
;; 						  ,(apply make-home-files (cfgset-home-files set))
;; 						  ,(apply make-home-config (cfgset-home-configs set))
;; 						  ,(apply make-home-desktop (cfgset-home-desktops set))
;; 						  ,(apply make-home-mime (cfgset-home-mimes set))))
;; 				      (cfgset-home-settings set))))
;; 	 (home-obj (eval home-expr (interaction-environment)))
;; 	 (transforms (cons (lambda (x) x) (cfgset-home-transforms set)))))
;;   ((apply compose transforms) home-obj))

(define-syntax make-home-environment
  (syntax-rules ()
    ((_ config-alist)
     (apply (lambda* (#:key
                      (packages '())
		      (services %base-home-services)
                      ;; 忽略其他未知参数
                      #:allow-other-keys
                      . rest)
	      (let ((services (if (null? services)
				  %base-home-services
				  services)))
		(home-environment
		 (packages packages)
		 (services services))))
            config-alist))))

(define (make-home set)
  (let* ((config-list (merge-config %default-config
				    `((services (,(apply make-home-env (cfgset-home-envs set))
						 ,(make-home-dotfiles %dotfilesdir)
						 ,(apply make-home-files (cfgset-home-files set))
						 ,(apply make-home-config (cfgset-home-configs set))
						 ,(apply make-home-desktop (cfgset-home-desktops set))
						 ,(apply make-home-mime (cfgset-home-mimes set)))))
				    (cfgset-home-settings set)))
	 (home-obj (make-home-environment (alist->keyword-list config-list)))
	 (transforms (cons (lambda (x) x) (cfgset-home-transforms set))))
    ((apply compose transforms) home-obj)))

(define (make-home-env . envs)
  (simple-service 'home-environment-variables
                  home-environment-variables-service-type
                  envs))

(define (make-home-dotfiles . dotfiles)
  (service home-dotfiles-service-type
           (home-dotfiles-configuration
            (directories dotfiles))))

(define (make-home-files . path-file-pairs)
  (simple-service 'chiko-home-files
                  home-files-service-type
                  path-file-pairs))

(define (make-home-config . path-file-pairs)
  (simple-service 'home-xdg-configuration
                  home-xdg-configuration-files-service-type
                  path-file-pairs))

(define (make-home-desktop . desktop-package-pairs)
  (simple-service 'home-desktop
                  home-files-service-type
                  (map (lambda (pair)
                         (let ((desktop (car pair))
			       (package (cadr pair)))
                           `(,(string-append ".local/share/applications/" desktop)
                             ,(computed-substitution-with-inputs 
			       desktop
			       (local-file (string-append %desktopdir desktop))
			       (list (spec->pkg package))))))
		       desktop-package-pairs)))

(define (make-home-mime . mimes)
  (simple-service 'default-mimes
                  home-xdg-configuration-files-service-type
                  `(("mimeapps.list"
                     ,(plain-file "mimeapps.list" 
                                  (string-join (cons "[Default Applications]" mimes) "\n"))))))
