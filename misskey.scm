(service misskey-service-type
    (misskey-configuration
      (image "ghcr.io/rakino/misskey:latest")
      (config
       `((url . "https://littlewing.yumieko.com")
         (port . 3000)
         (db
          . ((host . localhost)
             (port . 5432)
             (db . misskey)
             (user . misskey)
             (pass . ,(sops-str '("misskey" "password")))))
         (dbReplications . #f)
         (redis
          . ((host . localhost)
             (port . 6379)))
         (fulltextSearch
          . ((provider . sqlLike)))
         (id . "aid")
         (clusterLimit . 4)
         (outgoingAddressFamily . dual)
         (proxyRemoteFiles . #t)
         (signToActivityPubGet . #t)))))
