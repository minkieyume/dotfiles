;; 这是由图形安装程序生成的操作系统配置。
;;
;; 安装完成后，您可以学习并修改此文件以调整系统配置，
;; 并将其传递给 'guix system reconfigure' 命令以生效您的更改。


;; 指示要导入哪些模块以访问此配置中使用的变量。
(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
  (locale "zh_CN.utf8")
  (timezone "Asia/Singapore")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "yumieko")
  
  (initrd-modules (cons* "virtio_scsi" %base-initrd-modules))
  
  ;; 用户账户列表（'root' 是隐含的）。
  (users (cons* (user-account
                  (name "minkieyume")
                  (comment "Minkieyume")
                  (group "users")
                  (home-directory "/home/minkieyume")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; 以下是系统服务列表。要搜索可用
  ;; 服务，请在终端运行 'guix system search KEYWORD'。
  (services
   (append (list

                 ;; 要配置 OpenSSH，请将 'openssh-configuration'
                 ;; 记录作为第二个参数传递给下面的 'service'。
                 (service openssh-service-type
                    (openssh-configuration
                      (permit-root-login #t)))
                 (service dhcpcd-service-type)
                 (service ntp-service-type))

           ;; 这是我们正在追加的默认服务列表。
           %base-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "31481dcb-adb6-4939-9e3e-00816e884e0c"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

  ;; 这是被“挂载”的文件系统列表。唯一的
  ;; 文件系统标识符（“UUID”）可以通过
  ;; 在终端运行 'blkid' 获得。
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "DD36-2C2F"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems)))
