;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules machine chikocloud)
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
  #:use-module (chiko-modules sets web)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules user)
  #:use-module (chiko-modules user deploy)
  #:use-module (chiko-modules user root)
  #:use-module (chiko-modules user minkieyume)
  #:export (make-chikocloud-os))

(define %root
  (list (file-system
	 (mount-point "/")
	 (device (uuid "5e270b1a-b415-4956-9aff-ae400e2ff3f4"))
	 (type "ext4"))))

(define %machine-name
  "chikocloud")

(define %chikocloud-default-set
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
	 (bootloader grub-bootloader)
	 (targets (list "/dev/sda"))
	 (keyboard-layout keyboard-layout)))
      (initrd-modules ,%base-initrd-modules)
      (kernel-arguments ,%default-kernel-arguments)

      ;;存储设备及文件系统
      (file-systems ,(append %root %base-file-systems))

      ;;用户、组
      (users ,%base-user-accounts)
      (groups ,%base-groups)

      ;;特权程序及软件包
      (privileged-programs ,%default-privileged-programs)
      (packages ,%base-packages)))
   (sys-transforms
    (list (nonguix-transformation-linux)))))

(define %chikocloud-set
  (merge-sets
   %chikocloud-default-set
   (make-default-file-system-apps "c702c3a3-d8a4-456a-8e71-5134031222be" "minkieyume")
   (make-dhcpcd-networking %machine-name #:avahi? #t)
   (make-ipv6-gateway "2001:41d0:601:1100::592a/64" "2001:41d0:601:1100::1" "eth0")
   (make-guix '("--cores=0") #f)
   (make-container)
   (make-vps)
   ;; (make-postgresql-secret "misskey" "hedgedoc")
   ;; (make-webs (misskey-webserver "littlewing.yumieko.com"))
   ))

(define (make-chikocloud-os)
  (make-system %chikocloud-set))
