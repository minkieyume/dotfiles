(define-module (chiko-modules utils)
  #:export (merge-config
	    alist->keyword-list
	    valid-cfg?))

(define (alist->keyword-list alist)
  "将关联列表转换为关键字参数列表"
  (apply append
         (map (lambda (pair)
                (list (symbol->keyword (car pair))
                      (if (list? (cdr pair))
			  (cadr pair)
			  (cdr pair))))
              alist)))

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
  (append (filter (lambda (line)
		    (not (keycontain? cfg2 line))) cfg1)
	  (map (lambda (newcfg)
		 (merge-cfg-line cfg1 newcfg)) cfg2)))

(define (keycontain? lst item)
  (if (assq-ref lst (car item)) #t #f))

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
