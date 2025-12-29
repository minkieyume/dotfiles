(define-module (chiko-modules deploy chikopara)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (chiko-modules machine chikopara)
  #:export (%chikopara-deploy))

(define %chikopara-deploy
  (machine
   (operating-system (make-chikopara-os))
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
		   (host-name "chikopara")
		   (build-locally? #t)
		   (system "x86_64-linux")
		   (user "deploy")
		   (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAsA8jBgb45qWfTGX/zOmQY+zaIxq6we8tBOpe6wVkUu")
		   (port 22)))))
