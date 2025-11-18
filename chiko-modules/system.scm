;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules system)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (gnu packages linux)
  #:use-module (gnu services sysctl)
  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services networking)
  #:use-module (gnu services mcron)  
  #:use-module (gnu services mcron)
  #:use-module (gnu services ssh)
  #:use-module (rosenthal)
  #:use-module (rosenthal services networking)
  #:use-module (chiko services doas)
  #:use-module (chiko-modules utils)  
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules home)
  #:use-module (chiko-modules user)
  #:export (make-system))

(define %default-config
  `((sudoers-file
     ,(plain-file "sudoers" "Defaults env_reset\ndeploy ALL=(ALL) NOPASSWD: ALL"))
    (services ,(list (service pam-limits-service-type
  			      (list
			       (pam-limits-entry "*" 'both 'nofile 100000)))))
    (packages ,(specifications->packages '("unzip" "opendoas" "git" "openssl" "glances" "bind:utils" "rsync" "cryptsetup"
					   "fish" "btop" "curl" "neofetch" "gnunet" "tcpdump")))))
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

(define* (make-ssh keys #:optional (envs %default-ssh-allowed-env))
  (service openssh-service-type
	   (openssh-configuration
	    (password-authentication? #f)
	    (permit-root-login #f)
	    (authorized-keys keys)
	    (accepted-environment envs))))

(define (make-mcron . mjobs)
  (service mcron-service-type
	   (mcron-configuration
	    (jobs mjobs))))

(define (make-home-service . home-services)
  (service guix-home-service-type
	   home-services))

(define (make-doas . rules)
  (service doas-service-type
	   (doas-configuration
	    (rules (if (null? rules)
		       %default-doas-rules
		       rules)))))

(define (make-autoload-kernel-modules . modules)
  (service kernel-module-loader-service-type modules))

(define-syntax make-operating-system
  (syntax-rules ()
    ((_ config-alist)
     (apply (lambda* (#:key
                      ;; 内核相关
                      (kernel linux-libre)
                      (hurd #f)
                      (kernel-loadable-modules '())
                      (kernel-arguments %default-kernel-arguments)
                      
                      ;; 引导相关
                      bootloader
                      (label #f)
                      (keyboard-layout #f)
                      (initrd-modules %base-initrd-modules)
                      (initrd base-initrd)
                      (firmware %base-firmware)
                      
                      ;; 系统标识
                      host-name
                      
                      ;; 存储设备
                      (mapped-devices '())
                      file-systems
                      (swap-devices '())
                      
                      ;; 用户和组
                      (users %base-user-accounts)
                      (groups %base-groups)
                      (skeletons (default-skeletons))
                      
                      ;; 系统配置
                      ;;(issue %default-issue)
                      (packages %base-packages)
                      (timezone "Asia/Singapore")
                      (locale "zh_CN.utf8")
                      ;;(locale-definitions %default-locale-definitions)
                      ;;(locale-libcs (list glibc))
                      ;;(name-service-switch %default-nss)
                      
                      ;; 服务
                      (services %base-services)
                      (essential-services #f)
                      
                      ;; 权限和认证
                      (pam-services (base-pam-services))
                      (privileged-programs %default-privileged-programs)
                      (sudoers-file %sudoers-specification)
                      
                      ;; 忽略其他未知参数
                      #:allow-other-keys
                      . rest)
              (operating-system
               (kernel kernel)
               (hurd hurd)
               (kernel-loadable-modules kernel-loadable-modules)
               (kernel-arguments kernel-arguments)
               (bootloader bootloader)
               (keyboard-layout keyboard-layout)
               (initrd-modules initrd-modules)
               (initrd initrd)
               (firmware firmware)
               (host-name host-name)
               (mapped-devices mapped-devices)
               (file-systems file-systems)
               (swap-devices swap-devices)
               (users users)
               (groups groups)
               (skeletons skeletons)
               ;; (issue issue)
               (packages packages)
               (timezone timezone)
               (locale locale)
               ;; (locale-definitions locale-definitions)
               ;; (locale-libcs locale-libcs)
               ;; (name-service-switch name-service-switch)
               (services services)
               (pam-services pam-services)
               (privileged-programs privileged-programs)
               (sudoers-file sudoers-file)))
            config-alist))))

(define (make-system set)
  (let* ((config-list (merge-config %default-config
				    `((users ,(apply nuser-accounts (cfgset-user-list set)))
				      (groups ,(apply nuser-groups (cfgset-user-list set)))
				      (services (,(apply make-mcron (cfgset-mcron-jobs set))
						 ,(apply make-doas (cfgset-doas-rules set))
						 ,(make-ssh %ssh-keys)
						 ,(apply make-home-service (map (lambda (name)
										  `(,name
										    ,(make-home set)))
										(apply nuser-make-home-names (cfgset-user-list set)))))))
				    (cfgset-sys-settings set)))
	 (os-obj (make-operating-system (alist->keyword-list config-list)))
	 (transforms (cons (lambda (x) x) (cfgset-sys-transforms set))))
    ((apply compose transforms) os-obj)))
