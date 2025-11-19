(use-service-modules networking ssh)
(use-package-modules bootloaders)

(define %chikocloud
  (machine
    (operating-system (load "../reconfigure/chikocloud-system.scm"))
    (environment managed-host-environment-type)
    (configuration (machine-ssh-configuration
		     (host-name "chikocloud")
		     (build-locally? #f)
		     (system "x86_64-linux")
		     (user "deploy")
		     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBgcERFKrdhXOyA280PozUv8yrHOPqPGN+X4XhRn5EE8")
		     (port 22)))))

(define %chikopara
  (machine
    (operating-system (load "../reconfigure/chikopara-system.scm"))
    (environment managed-host-environment-type)
    (configuration (machine-ssh-configuration
		     (host-name "chikopara")
		     (build-locally? #t)
		     (system "x86_64-linux")
		     (user "deploy")
		     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAsA8jBgb45qWfTGX/zOmQY+zaIxq6we8tBOpe6wVkUu")
		     (port 22)))))

(define %chikoniko
  (machine
   (operating-system (load "../build/build-chikoniko.scm"))
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
		   (host-name "chikoniko")
		   (build-locally? #t)
		   (system "x86_64-linux")
		   (user "deploy")
		   (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDBw3VblSwKSmOPsjtE8wOoo+AJVWtJZ/S9SXW/d4v3V")
		   (port 22)))))

(define %chikoyumemi
  (machine
   (operating-system (load "../build/build-chikoyumemi.scm"))
   (environment managed-host-environment-type)
   (configuration (machine-ssh-configuration
		   (host-name "chikoyumemi")
		   (build-locally? #t)
		   (system "x86_64-linux")
		   (user "deploy")
		   (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAEqd0gx+mPuKJmi2ugGToNQD0zxVhqB7j5FJdMzzcV")
		   (port 22)))))

(list %chikocloud %chikoniko %chikopara %chikoyumemi)
