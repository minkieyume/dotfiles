(define-module (chiko-modules utils)
  #:export (merge-config
	    valid-cfg?))

(define %list-symbols
  '(initrd-modules kernel-loadable-modules
    kernel-arguments privileged-programs users groups
    services mapped-devices file-systems packages))

;; ((package ("pkg1" "pkg2")))
(define (merge-config . configs)
  (if (null? (cdr configs))
      (car configs)
      (merge-cfg (car configs)
		 (apply merge-config (cdr configs)))))

(define (merge-cfg cfg1 cfg2)
  (map (lambda (newcfg)
	 (merge-cfg-line cfg1 newcfg)) cfg2))

(define (merge-cfg-line source line)
  (let ((val (assq-ref source (car line))))
    (cond
     ((and (null? line) val) val)
     ((memq (car line) %list-symbols)
      (if val
	  (list (car line)
		(append (car val) (cadr line)))
	  line))
     (else line))))

(define valid-cfg?
  (lambda (cfg)
    (and (pair? cfg)
         (list? cfg))))
