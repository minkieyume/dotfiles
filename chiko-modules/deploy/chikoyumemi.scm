(define-module (chiko-modules deploy chikoyumemi)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (chiko-modules machine chikoyumemi)
  #:export (%chikoyumemi-deploy))

(define %chikoyumemi-deploy
  (machine
   (operating-system (make-chikoyumemi-os))
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
		   (host-name "chikoyumemi")
		   (build-locally? #t)
		   (system "x86_64-linux")
		   (user "deploy")
		   (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAEqd0gx+mPuKJmi2ugGToNQD0zxVhqB7j5FJdMzzcV")
		   (port 22)))))
