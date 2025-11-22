(define-module (chiko-modules deploy chikoniko)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (chiko-modules machine chikoniko)
  #:export (%chikoniko-deploy))

(define %chikoniko-deploy
  (machine
   (operating-system (make-chikoniko-os))
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
		   (host-name "chikoniko")
		   (build-locally? #t)
		   (system "x86_64-linux")
		   (user "deploy")
		   (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDBw3VblSwKSmOPsjtE8wOoo+AJVWtJZ/S9SXW/d4v3V")
		   (port 22)))))
