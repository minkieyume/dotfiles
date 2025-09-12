(define %chiko-channels
  (list (channel
         (name 'chikochannel)
         (url "https://github.com/minkieyume/chiko-guix-channel.git")
         (branch "main")
         (introduction
          (make-channel-introduction
           "a2138076f2e25810fb41aa545e3606c02eae6367"
           (openpgp-fingerprint
            "1D49 47B7 EC8F 8C3F 4B19  7077 9E91 3592 5221 70AD"))))
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
         (name 'guix)
         (url "https://git.guix.gnu.org/guix.git")
         (branch "master")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%chiko-channels
