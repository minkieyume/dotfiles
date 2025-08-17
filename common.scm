(use-modules (srfi srfi-1)
  (srfi srfi-26)
  (ice-9 match)
  (ice-9 popen)
  (ice-9 textual-ports)
  (guix diagnostics)
  (guix i18n)
  (guix store)
  (nonguix transformations)
  (rosenthal))

(define (chiko-path rel-path)
  (file-contents
    (string-append (dirname (current-filename)) "/" rel-path)))
