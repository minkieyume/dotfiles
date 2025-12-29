(define-module (chiko-modules packages server)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%server-toolkit))

(define %server-toolkit
  (specifications->packages
   '("glances"
     "emacs-no-x")))
