;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (rosenthal)
  #:use-module (gnu services networking)
  #:use-module (chiko services doas)
  #:use-module (gnu services mcron)
  #:use-module (rosenthal services networking)
  #:use-module (gnu services docker)
  #:use-module (gnu services mcron)
  #:use-module (gnu services ssh)
  #:use-module (chiko services doas)
  #:use-module (chiko-modules utils)
  #:export (make-system
	    %default-doas-rules
	    make-doas
	    make-mcron
	    make-ssh
	    make-home-service))

(define %default-config
  `((sudoers-file
     (plain-file "sudoers" "Defaults env_reset\ndeploy ALL=(ALL) NOPASSWD: ALL"))
    (services (list (service tailscale-service-type)
		    (service containerd-service-type)
		    (service docker-service-type
			     (docker-configuration
			       (enable-iptables? #f)))
		    (service pam-limits-service-type
  			     (list
			      (pam-limits-entry "*" 'both 'nofile 100000)))
		    (service ipfs-service-type
			     (ipfs-configuration
			       (package (spec->pkg "kubo"))
			       (gateway "/ip4/0.0.0.0/tcp/8880")
			       (api "/ip4/0.0.0.0/tcp/5001")))))
    (packages ,(specifications->packages '("kubo" "unzip" "opendoas" "git" "openssl" "glances" "gnupg" "bind:utils"
					   "rsync" "cryptsetup" "fish" "btop" "curl" "neofetch" "gnunet" "tcpdump")))))

(define %default-doas-rules
  (list
   (doas-rule
    (permit #t)
    (user ":wheel")
    (options '("persist" "keepenv")))
   (doas-rule
    (permit #t)
    (user ":wheel")
    (options '("persist"
               "setenv { http_proxy https_proxy HOME=/root XDG_CACHE_HOME=/root/.cache PATH=/run/setuid-programs:/root/.config/guix/current/bin:/run/current-system/profile/bin:/run/current-system/profile/sbin INFOPATH=/root/.config/guix/current/share/info:/run/current-system/profile/share/info GIT_EXEC_PATH=/root/.guix-profile/libexec/git-core}"))
    (as-target "root"))
   (doas-rule
    (permit #t)
    (user "root")
    (options '("nopass")))))

(define %default-ssh-allowed-env
  (list "LANG" "LC_*" "TZ" "PYTHONIOENCODING" "TERM" "COLORTERM"))

(define-syntax-rule (make-mcron mjobs ...)
  (service mcron-service-type
	   (mcron-configuration
	     (jobs (list mjobs ...)))))

(define %default-openssh-config
  '((password-authentication? #f)
    (permit-root-login #f)))

(define* (make-ssh-config keys #:optional (envs %default-ssh-allowed-env))
  (merge-config '() %default-openssh-config
		`((authorized-keys ,(cons list keys))
		  (accepted-environment ,(cons list envs)))))

(define-syntax make-ssh
  (lambda (stx)
    (syntax-case stx ()
      ((_ (keys ...))
       (let* ((vkeys (syntax->datum #'(keys ...)))
	      (merged (make-ssh-config vkeys)))
	 (with-syntax (((final-config ...) (datum->syntax stx merged)))
	   #'(service openssh-service-type
		      (openssh-configuration
			final-config ...)))))
      
      ((_ (keys ...) (allowed-environments ...))
       (let* ((vkeys (syntax->datum #'(keys ...)))
	      (venvs (syntax->datum #'(allowed-environments ...)))
	      (merged (make-ssh-config vkeys venvs)))
	 (with-syntax (((final-config ...) (datum->syntax stx merged)))
	   #'(service openssh-service-type
		      (openssh-configuration
			final-config ...))))))))

(define-syntax-rule (make-home-service (users ...) (environments ...))
  (service guix-home-service-type
  	 (list (list users environments) ...)))

(define-syntax make-doas
  (syntax-rules ()
    ((_)
     (service doas-service-type
	      (doas-configuration
	       (rules
		%default-doas-rules))))
    ((_ doas-rules ...)
     (service doas-service-type
	      (doas-configuration
	       (rules
		(list doas-rules ...)))))))

(define-syntax make-system
  (lambda (stx)
    (syntax-case stx ()
      ((_ (trans ...) (config ...))
       (let* ((cfgs (syntax->datum #'(config ...)))
	      (vcfgs (merge-config %default-config cfgs))
	      (stx-cfgs (datum->syntax stx (filter valid-cfg? vcfgs))))
	 (with-syntax (((config ...) stx-cfgs))
	   #'((compose (lambda (x) x) trans ...)
	      (operating-system
		config ...))))))))
