(use-modules
   (gnu home)                 ; 提供 home-environment
   (gnu home services)        ; 提供 home-service 的核心定义
   (gnu home services shells) ; 如果要配置 shell (bash、zsh)
   (gnu services)             ; 提供 service-type 与 service-extension
   (gnu packages admin)
   (guix gexp)
   (gnu packages emacs)
   (gnu packages version-control))
            
(home-environment
  
  (packages (list
    emacs
    git))
  (services (list
    (service home-bash-service-type
             (home-bash-configuration
              (guix-defaults? #t)
              (bash-profile (list (plain-file "bash-profile" "\n export HISTFILE=$XDG_CACHE_HOME/.bash_history")))
    	  (bashrc (list (plain-file "bashrc" (call-with-input-file "./config/bash/bashrc" get-string-all))))))
    (simple-service 'git-config home-files-service-type
    `((".gitconfig" ,(plain-file "gitconfig" (call-with-input-file "./config/gitconfig" get-string-all)))))
    (simple-service 'ssh home-files-service-type
                    `((".ssh/config" ,(plain-file "ssh-config" (call-with-input-file "./config/ssh-config" get-string-all))))))))
