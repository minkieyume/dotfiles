(define-module (chiko-modules deploy chikocloud)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (chiko-modules machine chikocloud)
  #:export (%chikocloud-deploy))

(define %chikocloud-deploy
  (machine
    (operating-system (make-chikocloud-os))
    (environment managed-host-environment-type)
    (configuration (machine-ssh-configuration
		     (host-name "100.96.116.126")
		     (build-locally? #t)
		     (system "x86_64-linux")
		     (user "deploy")
		     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBgcERFKrdhXOyA280PozUv8yrHOPqPGN+X4XhRn5EE8")
		     (port 22)))))
