(define-module (chiko-modules packages icecat)
  #:use-module (rosenthal)
  #:use-module (guix profiles)
  #:export (%icecat-extensions))

(define %icecat-extensions
  (specifications->packages
   '("icecat-l10n:zh-CN"
     "adaptive-tab-bar-colour-icecat"
     "livemarks-icecat"
     "miniflux-injector-icecat"
     "ohmyech-icecat"
     "privacy-redirect-icecat"
     "ublock-origin-icecat"
     )))
