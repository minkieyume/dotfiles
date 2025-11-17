(define-module (chiko-modules utils)
  #:export (merge-config
	    valid-cfg?))

(define (merge-config merge-symbols . configs)
  (if (null? (cdr configs))
      (car configs)
      (merge-cfg merge-symbols (car configs)
		 (apply merge-config (cons merge-symbols (cdr configs))))))

(define (merge-cfg merge-symbols cfg1 cfg2)
  (map (lambda (newcfg)
	 (merge-cfg-line merge-symbols cfg1 newcfg)) cfg2))

(define (merge-cfg-line merge-symbols source line)
  (let ((val (assq-ref (car line) source)))
    (cond
     ((and (null? line) val) val)
     ((memq (car line) merge-symbols)
      (if val
	  (list (car line)
		(append val (cadr line)))
	  line))
     (else line))))

(define valid-cfg?
  (lambda (cfg)
    (and (pair? cfg)
         (list? cfg))))
