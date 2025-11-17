(define-module (chiko-modules sets guix)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules loader guix-loader)
  #:export (make-guix))

(define (make-guix extra-options tmpdir)
  (make-cfgset*
   #:sys-transforms
   (list (lambda (os)
	   (operating-system
	    (inherit os)
	    (services
	     (modify-services (operating-system-user-services os)
  			      (guix-service-type
  			       config => (guix-configuration
					  (inherit config)
					  (substitute-urls %default-channels)
					  (channels %default-substitute-urls)
					  (authorized-keys %default-authorized-keys)
					  (discover? #t)
					  (extra-options extra-options)
					  (tmpdir tmpdir))))))))))
