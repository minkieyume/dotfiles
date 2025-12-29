;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules machine chikopara)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (gnu services networking)
  #:use-module (gnu services rsync)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (rosenthal)
  #:use-module (guix gexp)
  #:use-module (nonguix transformations)
  #:use-module (chiko-modules system)
  #:use-module (chiko-modules home)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules sets networking)
  #:use-module (chiko-modules sets guix)
  #:use-module (chiko-modules sets filesystem)
  #:use-module (chiko-modules sets container)
  #:use-module (chiko-modules sets secret)
  #:use-module (chiko-modules sets creator)
  #:use-module (chiko-modules sets server)
  #:use-module (chiko-modules sets nas)
  #:use-module (chiko-modules sets hardware)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules user)
  #:use-module (chiko-modules user deploy)
  #:use-module (chiko-modules user root)
  #:use-module (chiko-modules user minkieyume)
  #:export (make-chikopara-os))

(define %sabaru
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
               (device (file-system-label "sabaru"))
               (mount-point mount-point)
               (type "btrfs")
               (options (options-for-subvolume subvolume))
               (create-mount-point? #t)
               (check? (string=? mount-point "/")))))
      '(("@System" . "/")
        ("@Data"   . "/var/lib")
        ("@Home"   . "/home")
        (#f        . "/mnt/sabaru")))))

(define %boot
  (list
   (file-system
    (mount-point "/boot/efi")
    (device (uuid "2BB9-C31F"
                  'fat32))
    (type "vfat"))))

(define %nana
  (let ((options-for-subvolume
         (lambda (subvolume)
           (string-join
            (list "compress=zstd" "commit=3600"
                  (or (and=> subvolume (cut string-append "subvol=" <>))
                      "subvolid=5"))
            ","))))
    (map (match-lambda
           ((subvolume . mount-point)
            (file-system
             (device (file-system-label "nana"))
             (mount-point mount-point)
             (type "btrfs")
             (options (string-append (options-for-subvolume subvolume)))
  	   (flags '(lazy-time))
  	   (mount-may-fail? #t)
             (create-mount-point? #t)
             (check? (string=? mount-point "/")))))
	 '(("@Picture" . "/resource/picture")
           ("@Music"   . "/resource/music")
           ("@Film"   . "/resource/film")
           ("@Video" .  "/resource/video")
           ("@Download" . "/resource/download")
           ("@Develop" . "/resource/develop")
           ("@Book"   . "/resource/book")
           ("@Game"   . "/resource/game")
           ("@Creator"   . "/resource/creator")
           ("@Git"   . "/resource/git")
           ("@Res" . "/resource/res")
           ("@Data" . "/resource/data")
           (#f        . "/mnt/nana")))))

(define %riri
  (let ((options-for-subvolume
	 (lambda (subvolume)
           (string-join
            (list "compress=zstd" "commit=43200"
                  (or (and=> subvolume (cut string-append "subvol=" <>))
                      "subvolid=5"))
            ","))))
    (map (match-lambda
           ((subvolume . mount-point)
            (file-system
             (device (file-system-label "riri"))
             (mount-point mount-point)
             (type "btrfs")
             (options (string-append (options-for-subvolume subvolume)))
             (flags '(lazy-time))
             (mount-may-fail? #t)
             (create-mount-point? #t)
             (check? (string=? mount-point "/")))))
	 '(("@NanaBackup" . "/backup/nana")
           (#f        . "/mnt/riri")))))

(define %machine-name
  "chikopara")

(define %chikopara-default-set
  (cfgset
   (user-list (list (make-minkieyume) (make-deploy) (make-root)))
   (mcron-jobs `((job "0 2 * * *" "rsync -a /resource /backup/nana")))
   (sys-settings
    `((locale "zh_CN.utf8")
      (timezone "Asia/Singapore")
      (keyboard-layout ,(keyboard-layout "us"))
      (host-name ,%machine-name)
      
      ;;启动及内核配置
      (bootloader
       ,(bootloader-configuration
	 (bootloader grub-efi-bootloader)
	 (targets (list "/boot/efi"))
	 (keyboard-layout (keyboard-layout "us"))))
      (initrd-modules ,%base-initrd-modules)
      (kernel-arguments ,(append (list "kernel.sysrq=0"
				       "zswap.enabled=1"
				       "zswap.max_pool_percent=90"
				       "modprobe.blacklist=pcspkr")  %default-kernel-arguments))

      ;;存储设备及文件系统
      (file-systems ,(append %sabaru %boot %nana %riri %base-file-systems))

      ;;用户、组
      (users ,%base-user-accounts)
      (groups ,%base-groups)

      ;;特权程序及软件包
      (privileged-programs ,%default-privileged-programs)
      (packages ,%base-packages)))
   (sys-transforms
    (list (nonguix-transformation-linux)))))

(define %chikopara-set
  (merge-sets
   %chikopara-default-set
   (make-default-file-system-apps "d113fc46-c2e4-4211-87de-49892502b016" "minkieyume")
   (make-dhcpcd-networking %machine-name #:avahi? #t)
   (make-guix '("--cores=4") #f)
   (make-container)
   (make-singbox-listener)
   (make-base-server)
   (make-hdtoolkit)
   (make-postgresql-secret "immich")
   (make-rsync (rsync-module (name "backup")
  			     (file-name "/resource/data/backup/")
  			     (read-only? #f)))
   (make-nas)))

(define (make-chikopara-os)
  (make-system %chikopara-set))
