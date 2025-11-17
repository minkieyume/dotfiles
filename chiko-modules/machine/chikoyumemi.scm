;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules machine chikoyumemi)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (rosenthal)
  #:use-module (nonguix transformations)
  #:use-module (chiko-modules system)
  #:use-module (chiko-modules home)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader set-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules user)
  #:use-module (chiko-modules user deploy)
  #:use-module (chiko-modules user root)
  #:use-module (chiko-modules user minkieyume)
  #:export (%os))

(define %nvme0n1p1
  (let ((options-for-subvolume
	 (lambda (subvolume)
           (string-join
            (list "compress=zstd"
                  (or (and=> subvolume (cut string-append "subvol=" <>))
                      "subvolid=5"))
            ","))))
    (map (match-lambda
           ((subvolume . mount-point)
            (file-system
             (device (file-system-label "yumemi"))
             (mount-point mount-point)
             (type "btrfs")
             (options (options-for-subvolume subvolume))
             (create-mount-point? #t)
             (check? (string=? mount-point "/")))))
	 '(("@System" . "/")
           ("@Data"      .      "/var/lib")
           ("@Log"      .       "/var/log")
           ("@GNU"       .          "/gnu")
           ("@Home"      .         "/home")
           ("@Tmp"       .         "/tmp")
           ("@Creator"   . "/yumemi/creator")
           ("@Develop"   . "/yumemi/develop")
           ("@Picture"   . "/yumemi/picture")
           ("@Video"     . "/yumemi/video")
           ("@Audio"     .   "/yumemi/audio")
           ("@Program"   . "/yumemi/program")
           ("@Games"     .   "/yumemi/games")
	   ("@AI"  ."/yumemi/ai")
           ("@Download"  ."/yumemi/download")
           ("@Sandbox"   . "/yumemi/sandbox")
	   ("@Build"   . "/yumemi/build")
           (#f           .     "/mnt/yumemi")))))

(define %nvme0n1p2
  (list
   (file-system
    (mount-point "/boot/efi")
    (device (uuid "F13D-2F91"
  		'fat32))
    (type "vfat"))))

(define %galaxy
  (let ((options-for-subvolume
	 (lambda (subvolume)
           (string-join
            (list "compress=zstd"
                  (or (and=> subvolume (cut string-append "subvol=" <>))
                      "subvolid=5"))
            ","))))
    (map (match-lambda
           ((subvolume . mount-point)
            (file-system
             (device (file-system-label "galaxy"))
             (mount-point mount-point)
             (type "btrfs")
             (options (options-for-subvolume subvolume))
             (create-mount-point? #t)
	     (mount-may-fail? #t)
             (check? (string=? mount-point "/")))))
	 '(("@Games"     .   "/galaxy/games")
           ("@AI"  ."/galaxy/ai")
	   ("@Data" . "/galaxy/data")
           (#f           .     "/mnt/galaxy")))))

(define (make-chikoyumemi-config set)
  `((locale "zh_CN.utf8")
    (timezone "Asia/Singapore")
    (keyboard-layout (keyboard-layout "us"))
    (host-name "chikoyumemi")
    
    ;;启动及内核配置
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
    (initrd-modules (append (list <<initrd-module>>) %base-initrd-modules))
    (kernel-loadable-modules (specifications->packages (list <<kernel-loadable-module>>)))
    (kernel-arguments (append (list "kernel.sysrq=1"
				    "zswap.enabled=1"
				    "zswap.max_pool_percent=90")  %default-kernel-arguments))

    ;;存储设备及文件系统
    (mapped-devices '())
    (file-systems (append %nvme0n1p1 %nvme0n1p2 %galaxy %base-file-systems))

    ;;用户、组
    (users (append (nuser-accounts (make-minkieyume) (make-deploy) (make-root)) %base-user-accounts))
    (groups %base-groups)

    ;;特权程序及软件包
    (privileged-programs (cons* <<privileged-program>> %default-privileged-programs))
    (packages (append (specifications->packages '(<<package>>)) (list <<program>>) %base-packages))
    (services (cons* (make-doas)
		     (make-mcron ,cfgset-mcron-jobs)
		     (make-ssh)
		     (make-home-service (make-home))
  		   ((compose <<rosenthal-desktop-transformation>>) %rosenthal-desktop-services)))))

(define-syntax make-os
  (lambda (stx)
    (syntax-case stx ()
      ((_ set)
       (let* ((set-datum (syntax->datum #'set)))
	 (with-syntax (((config ...)
			(datum->syntax stx (merge-config (make-chikoyumemi-config set-datum)))))
	   #'(make-system ((nonguix-transformation-linux))
			  config ...)))))))

(define %os
  (make-os (make-set)))
