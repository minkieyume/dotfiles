;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets web)
  #:use-module (rosenthal)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services web)
  #:use-module (gnu services certbot)
  #:use-module (chiko services web)
  #:use-module (chiko services matrix)
  #:use-module (chiko services email)
  #:use-module (rosenthal services web)
  #:use-module (chiko-modules packages web)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules sets)
  #:use-module (guix records)
  #:export (<webserver>
	    webserver-cfgset
	    webserver-domains
	    webserver-nginx-servers
	    webserver-nginx-upstreams
	    webserver-nginx-stream-servers
	    webserver-nginx-stream-upstreams
	    make-webs
	    misskey-webserver
	    synapse-webserver
	    email-webserver
	    hedgedoc-webserver))

(define-record-type* <webserver>
  webserver make-webserver webserver?
  (wcfgset webserver-cfgset (default (cfgset)))
  (domains webserver-domains (default '()))
  (nginx-servers webserver-nginx-servers (default '()))
  (nginx-upstreams webserver-nginx-upstreams (default '()))
  (nginx-stream-upstreams webserver-nginx-stream-upstreams (default '()))
  (nginx-stream-servers webserver-nginx-stream-servers (default '())))

(define %nginx-proxy-confings
  (list "proxy_http_version 1.1;"
	"proxy_redirect off;"
	"proxy_set_header Host $host;"
	"proxy_set_header X-Real-IP $remote_addr;"
	"proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
	"proxy_set_header X-Forwarded-Proto $scheme;"
        "proxy_read_timeout 3600s;"
        "proxy_send_timeout 3600s;"
        "keepalive_timeout 120s;"
	"proxy_set_header Upgrade $http_upgrade;"
	"proxy_set_header Connection $connection_upgrade;"
	"proxy_buffering on;"
	"proxy_buffers 16 16k;"
	"proxy_buffer_size 32k;"
	"gzip on;"
	"gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;"
	"gzip_min_length 1024;"))

(define %nginx-cors-headers
  (list "add_header 'Access-Control-Allow-Origin' '*' always;"
	"add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS' always;"
	"add_header 'Access-Control-Allow-Headers' 'Origin, X-Requested-With, Content-Type, Accept, Authorization' always;"))

(define (make-webs . webservers)
  (merge-sets
   (apply merge-sets (map webserver-cfgset webservers))
   (cfgset
    (sys-settings `((services
		     ,(list (service certbot-service-type
				     (certbot-configuration
				       (email "sign@yumieko.com")
				       (certificates
					(map (compose
					      (lambda (d)
						(certificate-configuration (domains d)))
					      webserver-domains) webservers))))
			    (service nginx-service-type
				     (nginx-configuration
  				       (extra-content (string-append "map $http_upgrade $connection_upgrade { default upgrade; '' close; }\nclient_max_body_size 500M;"
								     "limit_req_zone $binary_remote_addr zone=appcreate:10m rate=10r/m;"))
  				       (server-blocks
  					(apply append (map webserver-nginx-servers webservers)))
				       (upstream-blocks
					(apply append (map webserver-nginx-upstreams webservers)))
				       (stream (nginx-stream-configuration
						(upstream-blocks
						 (apply append (map webserver-nginx-stream-upstreams webservers)))
						(server-blocks
						 (apply append (map webserver-nginx-stream-servers webservers))))))))))))))

(define (misskey-webserver domain)
  (webserver
   (wcfgset (cfgset
	     (mcron-jobs '((job "0 2 * * *" "doas -u misskey pg_dump -U misskey -d misskey > /tmp/misskey.sql && ( rsync -avz --timeout=30 /tmp/misskey.sql rsync://100.119.107.8/backup/misskey ; rm /tmp/misskey.sql)")
			   (job "0 2 * * *" "chmod -R 755 /var/lib/misskey/files")
			   (job "0 2 * * *" "rsync -avz --timeout=30 /var/lib/misskey/files rsync://100.119.107.8/backup/misskey")))
	     (sys-settings `((services ,(list (service misskey-service-type
  						       (misskey-configuration
  							(image "misskey/misskey:latest")
  							(postgresql-password-file "/etc/secret/postgres/misskey")
  							(config
							 `(("url" . ,(string-append "https://" domain))
							   ("port" . 3000)
							   ("db"
  							    ("host" . "localhost")
							    ("port" . 5432)
							    ("db" . "misskey")
							    ("user" . "misskey")
							    ("pass" . ,(secret-ref 'misskeydb)))
							   ("dbReplications" . #f)
							   ("redis"
							    ("host" . "localhost")
							    ("port" . 6379))
							   ("fulltextSearch"
							    ("provider" . "sqlLike"))
							   ("id" . "aidx")
							   ("clusterLimit" . 4)
							   ("outgoingAddressFamily" . "dual")
							   ("proxyRemoteFiles" . #t)
							   ("signToActivityPubGet" . #t)))))))))))
   (domains (list domain))
   (nginx-servers (list (nginx-server-configuration
			  (server-name (list domain))
			  (listen '("443 ssl" "[::]:443 ssl"))
			  (ssl-certificate (string-append "/etc/certs/" domain "/fullchain.pem"))
			  (ssl-certificate-key (string-append "/etc/certs/" domain "/privkey.pem"))
			  (locations
			   (list
			    (nginx-location-configuration
			      (uri "/")
			      (body `(,@%nginx-proxy-confings
				      "proxy_pass http://127.0.0.1:3000;")))
			    (nginx-location-configuration
			      (uri "/api/app/create")
			      (body `(,@%nginx-proxy-confings
				      "limit_req zone=appcreate burst=5 nodelay;"
				      "proxy_pass http://127.0.0.1:3000;")))
			    (nginx-location-configuration
			      (uri (secret-ref 'ws-transport-path))
			      (body `(,@%nginx-proxy-confings
				      "proxy_pass http://127.0.0.1:7890;"))))))))))

(define (synapse-webserver domain)
  (webserver
   (wcfgset (cfgset
	     (mcron-jobs '((job "0 2 * * *" "doas -u synapse pg_dump -U synapse -d synapse > /tmp/synapse.sql && ( rsync -avz --timeout=30 /tmp/synapse.sql rsync://100.119.107.8/backup/synapse ; rm /tmp/synapse.sql)")
			   (job "0 2 * * *" "rsync -avz --timeout=30 /var/lib/synapse rsync://100.119.107.8/backup")))
	     (sys-settings `((services ,(list (service synapse-service-type
						       (synapse-configuration
							(uid 991)
							(gid 991)
							(server-name domain)
							(postgresql-password-file "/var/lib/synapse/ppasu")))))))))
   (domains (list domain))
   (nginx-servers (list (nginx-server-configuration
			 (server-name (list domain))
			 (listen '("443 ssl" "[::]:443 ssl"))
			 (ssl-certificate (string-append "/etc/certs/" domain "/fullchain.pem"))
			 (ssl-certificate-key (string-append "/etc/certs/" domain "/privkey.pem"))
			 (locations
			  (list
			   (nginx-location-configuration
			    (uri "/")
			    (body `(,@%nginx-proxy-confings
				    "proxy_pass http://127.0.0.1:8008;")))
			   (nginx-location-configuration
			    (uri "/_matrix/media")
			    (body `(,@%nginx-proxy-confings
				    "proxy_pass http://127.0.0.1:8008;")))
			   (nginx-location-configuration
			    (uri "/.well-known/matrix/server")
			    (body `("add_header Access-Control-Allow-Origin *;"
				    "add_header Access-Control-Allow-Methods 'GET, OPTIONS';"
				    "default_type application/json;"
				    ,(string-append "return 200 '{\"m.server\": {\"base_url\": \"" domain ":443\"}}';"))))
			   (nginx-location-configuration
			    (uri "/.well-known/matrix/client")
			    (body `("add_header Access-Control-Allow-Origin *;"
				    "add_header Access-Control-Allow-Methods 'GET, OPTIONS';"
				    "default_type application/json;"
				    ,(string-append "return 200 '{\"m.homeserver\": {\"base_url\": \"https://" domain "\"}}';")))))))
			(nginx-server-configuration
			 (server-name (list domain))
			 (listen '("8448 ssl" "[::]:8448 ssl"))
			 (ssl-certificate (string-append "/etc/certs/" domain "/fullchain.pem"))
			 (ssl-certificate-key (string-append "/etc/certs/" domain "/privkey.pem"))
			 (locations
			  (list
			   (nginx-location-configuration
			    (uri "/")
			    (body `(,@%nginx-proxy-confings
  				    "proxy_pass http://127.0.0.1:8008;"))))))))))

(define (email-webserver domain @xxxx)
  (webserver
   (wcfgset (cfgset
	     (mcron-jobs '((job "0 2 * * *" "rsync -avz --timeout=30 /var/lib/docker-mailserver rsync://100.119.107.8/backup")))
	     (sys-settings `((services ,(list (service docker-mailserver-service-type
      						       (docker-mailserver-configuration
    							(hostname domain)
    							(ssl-cert-path "/etc/letsencrypt")
							(host-mode? #t)
  							(use-rspamd? #t)
  							(rspamd-extenal-redis? #t)
      							(ports `(("8443" . "443")
    								 ("8080" . "8080")
    								 ("25" . "25")
    								 ("587" . "587")
    								 ("465" . "465")
    								 ("143" . "143")
    								 ("993" . "993")
    								 ("4190" . "4190")
    								 ("110" . "110")
    								 ("995" . "995")))
    							(environment `(("PERMIT_DOCKER" . "none")
  								       ("POSTMASTER_ADDRESS" . ,(string-append "postmaster@" @xxxx))
    								       ("SSL_CERT_PATH" . ,(string-append "/certs/live/" domain "/fullchain.pem"))
    								       ("SSL_KEY_PATH" . ,(string-append "/certs/live/" domain "/privkey.pem"))
    								       ("ENABLE_LDAP" . "0")
  								       ("ENABLE_CLAMAV" . "1")
    								       ("ENABLE_SPAMASSASSIN" . "0")
    								       ("ENABLE_FAIL2BAN" . "1")  			 
  								       ("ONE_DIR" . "1")
    								       ("DMS_DEBUG" . "0")))))))))))
   (domains (list domain))
   (nginx-servers (list (nginx-server-configuration
			 (server-name (list domain))
			 (listen '("443 ssl" "[::]:443 ssl"))
			 (ssl-certificate (string-append "/etc/certs/" domain "/fullchain.pem"))
			 (ssl-certificate-key (string-append "/etc/certs/" domain "/privkey.pem"))
			 (locations
			  (list
			   (nginx-location-configuration
			    (uri "/")
			    (body `("proxy_pass http://127.0.0.1:8080;"
				    ,@%nginx-proxy-confings))))))))))

(define (hedgedoc-webserver)
  (webserver
   (wcfgset (cfgset
	     (sys-settings `((services ,(list (service hedgedoc-service-type
  						       (hedgedoc-configuration
  							(uid 911)
  							(gid 911)
  							(port 3012)
  							(db-pass (secret-ref 'hedgedoc-db-pass))
  							(postgresql-password-file "/etc/secret/postgres/hedgedoc")
  							(hostname "doc.yumieko.com")
  							(environment
  							 `(("CMD_EMAIL" . "true")
							   ("CMD_ALLOW_EMAIL_REGISTER" . "false")
							   ("CMD_ALLOW_FREEURL" . "true")
							   ("CMD_REQUIRE_FREEURL_AUTHENTICATION" . "true")
							   ("CMD_DEFAULT_PERMISSION" . "editable")
  							   ("CMD_ALLOW_ANONYMOUS" . "false")
  							   ("CMD_PROTOCOL_USESSL" . "true")
  							   ("CMD_SESSION_SECRET" . ,(secret-ref 'hedgedoc-session-secret))
  							   ("CMD_OAUTH2_USER_PROFILE_URL" . "https://littlewing.yumieko.com/api/i")
  							   ("CMD_OAUTH2_USER_PROFILE_USERNAME_ATTR" . "username")
  							   ("CMD_OAUTH2_USER_PROFILE_DISPLAY_NAME_ATTR" . "name")
  							   ("CMD_OAUTH2_USER_PROFILE_EMAIL_ATTR" . "email")
  							   ("CMD_OAUTH2_USER_PROFILE_ID_ATTR" . "id")
  							   ("CMD_OAUTH2_TOKEN_URL" . "https://littlewing.yumieko.com/oauth/token")
  							   ("CMD_OAUTH2_AUTHORIZATION_URL" . "https://littlewing.yumieko.com/oauth/authorize")
  							   ;;("CMD_OAUTH2_CLIENT_ID" . "https://doc.yumieko.com/oauth-client/client.html")
  							   ("CMD_OAUTH2_CLIENT_SECRET" . "")
  							   ("CMD_OAUTH2_PROVIDERNAME" . "Misskey")
  							   ("CMD_OAUTH2_SCOPE" . "read:account")))))))))))
   (domains (list "doc.yumieko.com"))
   (nginx-servers (list (nginx-server-configuration
			 (server-name '("doc.yumieko.com"))
			 (listen '("443 ssl" "[::]:443 ssl"))
			 (ssl-certificate "/etc/certs/doc.yumieko.com/fullchain.pem")
			 (ssl-certificate-key "/etc/certs/doc.yumieko.com/privkey.pem")
			 (locations
			  (list
			   (nginx-location-configuration
			    (uri "/")
			    (body `(,@%nginx-proxy-confings
				    "proxy_pass http://127.0.0.1:3012;"))))))))))
