(use-modules (gnu)
(gnu system)
(gnu service networking)  
(gnu service ssh)
(gnu packages emacs))

(operating-system
  (locale "zh_CN.utf8")
(timezone "Asia/Singapore")
(keyboard-layout (keyboard-layout "us"))
(host-name "chikocloud")
  (bootloader (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets (list "/boot/efi"))
  (keyboard-layout keyboard-layout)))
  (initrd-modules (cons* "virtio_scsi" %base-initrd-modules))
  (users (cons* (user-account
  (name "minkieyume")
  (comment "Minkieyume")
  (group "users")
  (home-directory "/home/minkieyume")
  (supplementary-groups '("wheel" "netdev" "audio" "video"))) %base-user-accounts))
  (services (cons* (service dhcpcd-service-type)
(service ntp-service-type)
(service openssh-service-type
  (openssh-configuration
    (permit-root-login #t)
    (authorized-keys
      `(("minkieyume"
          ,(local-file "../../files/keys/yumemi_rsa.pub"))))))
(service doas-service-type) %base-services))
  (mapped-devices (list (mapped-device
  (source (uuid
            "31481dcb-adb6-4939-9e3e-00816e884e0c"))
  (target "cryptroot")
  (type luks-device-mapping))))
  (file-systems (cons* (file-system
  (mount-point "/boot/efi")
  (device (uuid "DD36-2C2F"
            'fat32))
  (type "vfat"))
(file-system
  (mount-point "/")
  (device "/dev/mapper/cryptroot")
  (type "ext4")
  (dependencies mapped-devices)) %base-file-systems))
  (packages (cons* htop
doas
emacs %base-packages)))
