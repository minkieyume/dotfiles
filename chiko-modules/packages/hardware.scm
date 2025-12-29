(define-module (chiko-modules packages hardware)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%hd-toolkit))

(define %hd-toolkit
  (specifications->packages
   '("smartmontools"
     "iotop"
     "hdparm")))
