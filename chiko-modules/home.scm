(define-module (chiko-modules home)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:export (make-home
	    make-home-env
	    make-home-dotfiles
	    make-home-files
	    make-home-config
	    make-home-desktop
	    make-home-mime))

(define %default-config
  `((packages ())
    (services ())))

(define-syntax make-home
  (lambda (stx)
    (syntax-case stx ()
      ((_ (trans ...) (config ...))
       (let* ((cfgs (syntax->datum #'(config ...)))
	      (vcfgs (merge-config %default-config cfgs))
	      (stx-cfgs (datum->syntax stx (filter valid-cfg? vcfgs))))
	 (with-syntax (((config ...) stx-cfgs))
	   #'((compose (lambda (x) x) trans ...)
	      (home-environment
	       config ...))))))))

(define-syntax-rule (make-home-env env ...)
  (simple-service 'home-environment-variables
		  home-environment-variables-service-type
		  (list env ...)))

(define-syntax-rule (make-home-dotfiles dotfiles ...)
  (service home-dotfiles-service-type
	   (home-dotfiles-configuration
  	  (directories (list dotfiles ...)))))

(define-syntax-rule (make-home-files (path file) ...)
  (simple-service 'chiko-home-files
		  home-files-service-type
		  `((path file)  ...)))

(define-syntax-rule (make-home-config (path file) ...)
  (simple-service 'home-xdg-configuration
  		home-xdg-configuration-files-service-type
		  `((path file)  ...)))

(define-syntax-rule (make-home-desktop (desktop package) ...)
  (simple-service 'home-desktop
  		home-files-service-type
  		`((,(string-append ".local/share/applications/" desktop)
		     ,(computed-substitution-with-inputs (string-append desktop)
  						       (local-file (string-append %desktopdir desktop))
  						       (list (spec->pkg package)))) ...)))
(define-syntax-rule (make-home-mime mime ...)
  (simple-service 'default-mimes
  		home-xdg-configuration-files-service-type
  		`(("mimeapps.list"
		     ,(plain-file "mimeapps.list" (string-join '("[Default Applications]" mime ...) "\n"))))))
