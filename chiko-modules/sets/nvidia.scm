(define-module (chiko-modules sets networking)
  #:use-module (nonguix transformations)
  #:use-module (nongnu packages nvidia)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:export (make-nvidia))

(define (make-nvidia)
  (make-cfgset*
   #:sys-transforms
   (list replace-mesa
	 (nonguix-transformation-nvidia)
	 (lambda (os)
	   (operating-system
	    (inherit os)      
	    (kernel-arguments
	     (cons* "modprobe.blacklist=pcspkr,nouveau"
		    "nvidia_drm.modeset=1"
		    (operating-system-user-kernel-arguments os))))))))
