;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules machine chikoniko)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
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
  #:use-module (chiko-modules sets desktop)
  #:use-module (chiko-modules sets nvidia)
  #:use-module (chiko-modules sets guix)
  #:use-module (chiko-modules sets emacs)
  #:use-module (chiko-modules sets daily)
  #:use-module (chiko-modules sets filesystem)
  #:use-module (chiko-modules sets container)
  #:use-module (chiko-modules sets secret)
  #:use-module (chiko-modules sets appstore)
  #:use-module (chiko-modules sets video)
  #:use-module (chiko-modules sets creator)
  #:use-module (chiko-modules sets game)
  #:use-module (chiko-modules sets ai)
  #:use-module (chiko-modules sets develop)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules user)
  #:use-module (chiko-modules user deploy)
  #:use-module (chiko-modules user root)
  #:use-module (chiko-modules user minkieyume)
  #:export (make-chikoniko-os))

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
             (device (file-system-label "niko"))
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
           ("@Tmp"       .          "/tmp")
           ("@Creator"   . "/niko/creator")
           ("@Develop"   . "/niko/develop")
           ("@Picture"   . "/niko/picture")
           ("@Video"     .   "/niko/video")
           ("@Audio"     .   "/niko/audio")
           ("@Program"   . "/niko/program")
           ("@Games"     .   "/niko/games")
           ("@Download"  ."/niko/download")
           ("@Sandbox"   . "/niko/sandbox")
           ("@Build"     .   "/niko/build")
           (#f           .     "/mnt/niko")))))

(define %nvme0n1p2
  (list
   (file-system
    (mount-point "/boot/efi")
    (device (uuid "639A-B4E6"
  		'fat32))
    (type "vfat"))))

(define %machine-name
  "chikoniko")

(define %chikoniko-default-set
  (cfgset
   (user-list (list (make-minkieyume) (make-deploy) (make-root)))
   (home-files `(("Downloads" ,(symlink-to "/niko/download"))
		 ("Pictures" ,(symlink-to "/niko/picture"))
		 ("Creator" ,(symlink-to "/niko/creator"))
		 ("Develop" ,(symlink-to "/niko/develop"))
		 ("Application" ,(symlink-to "/niko/program"))
		 ("Audio" ,(symlink-to "/niko/audio"))
		 ("Video" ,(symlink-to "/niko/video"))
		 ("Games" ,(symlink-to "/niko/games"))
		 ;; ;;加载文件
		 (".dash_rsa" ,(local-file (string-append
					    %secretdir
					    "keys/dash_rsa")))
		 (".gitconfig" ,(local-file (string-append
					     %configdir
					     "gitconfig")))
		 (".ssh/config" ,(local-file (string-append
					      %configdir
					      "ssh-config")))
		 (".ssh/environment" ,(local-file (string-append
						   %configdir
						   "ssh-env")))))
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
      (kernel-arguments ,(append (list "i8042.reset" "i8042.nomux" "i8042.nopnp"
				       "kernel.sysrq=1"
				       "zswap.enabled=1"
				       "zswap.max_pool_percent=90"
				       "modprobe.blacklist=pcspkr")  %default-kernel-arguments))

      ;;存储设备及文件系统
      (file-systems ,(append %nvme0n1p1 %nvme0n1p2 %base-file-systems))

      ;;用户、组
      (users ,%base-user-accounts)
      (groups ,%base-groups)

      ;;特权程序及软件包
      (privileged-programs ,%default-privileged-programs)
      (packages ,%base-packages)))
   (sys-transforms
    (list (nonguix-transformation-linux)))))

(define %chikoniko-set
  (merge-sets
   %chikoniko-default-set
   (make-default-file-system-apps "204fc3c3-89b6-449b-9b32-9df5ed18d024" "minkieyume")
   (make-nm-trans-networking %machine-name)
   (make-guix '("--cores=4") "/mnt/niko/@Build")
   (make-wayland-desktop %machine-name #:background (make-swaybg %machine-name
								 (local-file (string-append
									      %secretdir "wallpapers/wallpaper.png"))))
   (make-container)
   (make-gpg-agent)
   (make-keepassxc)
   (make-emacs %machine-name)
   (make-daily)
   (make-flatpak (apply make-flatpak-desktops %default-flatpak-desktops))
   (make-video)
   (make-creator)
   (make-game)
   (make-develop)
   (make-aider)))

(define (make-chikoniko-os)
  (make-system %chikoniko-set))
