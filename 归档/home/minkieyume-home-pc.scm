(use-modules (gnu)
(gnu home)
(gnu home services)
(gnu home services dotfiles)
(gnu home services shells)
(gnu services)
(gnu packages admin)
(guix gexp)
(guix utils)
(srfi srfi-26)
(ice-9 match)
(rosenthal))


(define minkieyume-home-pc
  (home-environment
    
    (packages (append (specifications->packages (list )) (load "../packages/emacs.scm")))
    (services (cons* (service home-dotfiles-service-type
	 (home-dotfiles-configuration
	  (directories '("../files/config/dotfiles"))))
(simple-service 'home-environment-variables
		home-environment-variables-service-type
		`(("EDITOR" . "emacsclient")
("VISUAL" . "$EDITOR")
("ESHELL" . ,(file-append (spec->pkg "fish") "/bin/fish"))))
(simple-service 'emacs-configuration
      		home-xdg-configuration-files-service-type
      		`(("emacs/init.el"
      		   ,(computed-substitution-with-inputs "init.el"
      						       (local-file "../files/config/emacs/init.el")
						       (append
							(list (local-file "../files/config/emacs" #:recursive? #t)
							      (local-file "../secret/epasu" #:recursive? #t))
							(specs->pkgs "ccls"
      								     "fish"
      								     "python-lsp-server"
      								     "zig-zls"
    								     "fd"
								     "mpv"
								     "ffmpegthumbnailer"
								     "7zip"
								     "imagemagick"
								     "godot"
								     "mediainfo"
								     "vips"))))
      		  ("emacs/.init-themes.el"
      		   ,(local-file "../files/config/chikoyumemi/init-theme.el"))))
(simple-service 'home-emacs
		home-shepherd-service-type
		(list (shepherd-service
		       (provision '(emacs-daemon))
		       (start
			#~(make-forkexec-constructor
			   '("emacs" "--fg-daemon")))
		       (stop
			#~(make-forkexec-constructor
			   '("emacsclient" "--eval" "(kill-emacs)"))))))
(simple-service 'emacs-auth-info
		home-files-service-type
		`((".authinfo.gpg" ,(local-file "../secret/authinfo.gpg")))) %rosenthal-desktop-home-services))))
