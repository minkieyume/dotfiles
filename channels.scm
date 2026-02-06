;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define %chiko-channels
  (list (channel
          (name 'chikochannel)
          (url "https://codeberg.org/minkieyume/chiko-guix-channel.git")
          (branch "main")
          (introduction
           (make-channel-introduction
            "f7a3b72ca1c00106b93b993fe0424f2278d237c2"
            (openpgp-fingerprint
             "46CA F209 C8FA D487 3152  240B 3611 1AAE FB96 D7D1"))))
	(channel
	  (name 'rosenthal)
	  (url "https://codeberg.org/hako/rosenthal.git")
	  (branch "trunk")
	  (introduction
	   (make-channel-introduction
	    "7677db76330121a901604dfbad19077893865f35"
	    (openpgp-fingerprint
	     "13E7 6CD6 E649 C28C 3385  4DF5 5E5A A665 6149 17F7"))))
	(channel
	  (name 'nonguix)
	  (url "https://github.com/nonguix/nonguix.git")
	  ;; Enable signature verification:
	  (introduction
	   (make-channel-introduction
	    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
	    (openpgp-fingerprint
	     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
	(channel
	  (name 'guixcn)
	  (url "https://codeberg.org/guixcn/guix-channel.git")
	  (introduction
	   (make-channel-introduction
            "993d200265630e9c408028a022f32f34acacdf29"
            (openpgp-fingerprint
             "7EBE A494 60CE 5E2C 0875  7FDB 3B5A A993 E1A2 DFF0"))))
	(channel
	  (name 'rustup)
	  (url "https://github.com/declantsien/guix-rustup")
	  (introduction
	   (make-channel-introduction
	    "325d3e2859d482c16da21eb07f2c6ff9c6c72a80"
	    (openpgp-fingerprint
	     "F695 F39E C625 E081 33B5  759F 0FC6 8703 75EF E2F5"))))
	(channel
          (name 'guix)
          (url "https://git.guix.gnu.org/guix.git")
          (branch "master")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%chiko-channels
