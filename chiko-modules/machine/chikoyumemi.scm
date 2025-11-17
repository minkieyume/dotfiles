;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules machine chikoyumemi)
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
  #:use-module (chiko-modules sets networking)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules user)
  #:use-module (chiko-modules user deploy)
  #:use-module (chiko-modules user root)
  #:use-module (chiko-modules user minkieyume)
  #:export (make-chikoyumemi-os))

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

(define %machine-name
  "chikoyumemi")

(define %chikoyumemi-set
  (merge-sets
   (make-nm-trans-networking %machine-name)))

(define (make-chikoyumemi-system-set set)
  (cfgset
   (user-list (list (make-minkieyume) (make-deploy) (make-root)))
   (sys-settings
    `((locale "zh_CN.utf8")
      (timezone "Asia/Singapore")
      (keyboard-layout (keyboard-layout "us"))
      (host-name %machine-name)
      
      ;;启动及内核配置
      (bootloader
	(bootloader-configuration
	  (bootloader grub-efi-bootloader)
	  (targets (list "/boot/efi"))
	  (keyboard-layout keyboard-layout)))
      (initrd-modules (list ,@%base-initrd-modules))
      (kernel-loadable-modules '())
      (kernel-arguments (list ,@(append (list "kernel.sysrq=1"
					      "zswap.enabled=1"
					      "zswap.max_pool_percent=90")  %default-kernel-arguments)))

      ;;存储设备及文件系统
      (mapped-devices '())
      (file-systems (list ,@(append %nvme0n1p1 %nvme0n1p2 %galaxy %base-file-systems)))

      ;;用户、组
      (users (list ,@%base-user-accounts))
      (groups (list ,@%base-groups))

      ;;特权程序及软件包
      (privileged-programs (list ,@%default-privileged-programs))
      (packages (list ,@%base-packages))
      (services (list (make-ssh (%chiko-ssh-key))
		      (make-home-service ("minkieyume" (make-home-from-set ,set)))))))
   (sys-transforms
    (list (nonguix-transformation-linux)))))

(define %chikoyumemi-os-set
  (merge-sets (make-chikoyumemi-system-set %chikoyumemi-set) %chikoyumemi-set))

(define (make-chikoyumemi-os)
  (make-system %chikoyumemi-os-set))
