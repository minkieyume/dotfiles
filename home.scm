(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages admin)
	     (gnu packages emacs)
	     (gnu packages version-control)
             (guix gexp))

(home-environment
 (packages  (list emacs git misskey))
 (services
  (cons*
   (load "misskey.scm")
   (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\n export HISTFILE=$XDG_CACHE_HOME/.bash_history")))	          
	     (bashrc (list (plain-file "bashrc" "\nexport TERM=xterm-256color \nexport GUIX_PROFILE=$HOME/.guix-profile \n. $GUIX_PROFILE/etc/profile\nexport GUIX_LOCPATH=$HOME/.guix-profile/lib/locale")))))
   (simple-service 'git-config
                   home-files-service-type
                   `(("gitconfig" ,(plain-file "gitconfig"
"  
[user]
  name = Minkie Yume
  email = minkie@example.com

[core]
  editor = emacs

[color]
  ui = auto
"))))
   (simple-service 'ssh
                home-files-service-type
                `(("ssh/config" ,(plain-file "ssh-config"
"Host *
  AddKeysToAgent yes
  IdentityFile ~/.ssh/id_rsa
"))))))))

