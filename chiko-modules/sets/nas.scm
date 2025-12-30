;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets nas)
  #:use-module (rosenthal)
  #:use-module (rosenthal services web)
  #:use-module (gnu services)
  #:use-module (chiko services web)
  #:use-module (chiko-modules packages nas)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules sets)
  #:export (make-nas))

(define (make-phodav path)
  (simple-service 'phodav-service
  		shepherd-root-service-type
  		(list
  		 (let ((port 8990)
			 (path path)
			 (users '(("chiko" . "09ed42cad7c512496c9de58d499ce426")))
			 (no-mdns #t)) ;; set to #t to disable mdns
  		   (shepherd-service
  		    (documentation "Run Phodav Mount")
  		    (provision '(phodav chezdav))
  		    (requirement '(networking))
		      (respawn? #t)
  		    (respawn-limit 100)
  		    (respawn-delay 20)
  		    (start #~(make-forkexec-constructor
  			      (list #$(file-append (spec->pkg "phodav") "/bin/chezdav")
				      "--port" #$(number->string port)
				      "--path" #$path				    
				      "--htdigest" #$(plain-file "phodav-htdigest.conf" (string-join
											 (map
											  (lambda (u)
											    (string-append (car u)
													   ":Phodav:" (or (cdr u) ""))) users)
											 "\n"))
				      "--realm" "Phodav"
				      "--public"
				      #$(if no-mdns "--no-mdns" ""))
				#:log-file "/var/log/phodav.log"))
  		    (stop #~(make-kill-destructor)))))))

(define (make-navidrome extra-config)
  (service navidrome-service-type
	   (navidrome-configuration
	    (extra-config extra-config))))

(define (make-calibre)
  (service calibre-web-service-type
  	 (calibre-web-configuration
  	  (port 8083))))

(define (make-immich)
  (service immich-service-type
  	 (immich-configuration
  	  (db-password (secret-ref 'immich-db-pass))
	    (postgresql-password-file "/etc/secret/postgres/immich"))))

(define* (make-nas #:key
		   (phodav-path "/resource")
		   (navidrome-config
		    (string-append "LogLevel = 'DEBUG'\n"
				   "Scanner.Schedule = '@every 24h'\n"
  				   "TranscodingCacheSize = '150MiB'\n"
				   "MusicFolder = '/resource/music/musics'")))
  (cfgset
   (sys-settings `((services
		    ,(list (make-phodav phodav-path)
			   (make-navidrome navidrome-config)
			   (make-calibre)
			   (make-immich)))))))
