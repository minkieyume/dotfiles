;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets secret)
  #:use-module (guix gexp)
  #:use-module (rosenthal)
  #:use-module (gnu home services gnupg)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules loader secret-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages desktop)
  #:export (make-gpg-agent
	    make-keepassxc
	    make-postgresql-secret))

(define* (make-gpg-agent #:key
			 (pinetry "pinentry-emacs")
			 (extra-options '("allow-emacs-pinentry"
					  "allow-loopback-pinentry")))
  (cfgset
   (home-settings `((services
		     ,(list (service home-gpg-agent-service-type
				     (home-gpg-agent-configuration
				       (pinentry-program
					(file-append (spec->pkg pinetry) (string-append "/bin/" pinetry)))
				       (ssh-support? #t)
				       (extra-content (string-join extra-options "\n"))))))))
   ;; (home-files `((".gnupg/sshcontrol"
   ;; 		  ,(string-append
   ;; 		    %configdir
   ;; 		    "sshcontrol"))))
   (sys-settings `((packages ,(specifications->packages (list pinetry)))))))

(define (make-keepassxc)
  (cfgset
   (sys-settings `((packages
		    ,(specifications->packages '("libsecret"
						 "keepassxc")))))))

(define (make-postgresql-secret . needed)
  (let ((secrets `(("forgejo" . ("secret/postgres/forgejo"
				 ,(plain-file "forgejo-db-password.txt"
					      (secret-ref 'forgejo-db))))
		   ("immich" . ("secret/postgres/immich"
				 ,(plain-file "immich-db-password.txt"
					      (secret-ref 'immich-db-pass))))
		   ("misskey" . ("secret/postgres/misskey"
				 ,(plain-file "misskey-db-password.txt"
					      (secret-ref 'misskeydb))))
		   ("hedgedoc" . ("secret/postgres/hedgedoc"
				  ,(plain-file "hedgedoc-db-password.txt"
					       (secret-ref 'hedgedoc-db-pass)))))))
    (cfgset
     (sys-settings `((services
		      ,(list (simple-service 'postgres-pass-file
					     etc-service-type
					     (map
					      (lambda (item) (cdr item))
					      (filter (lambda (item)
							(if (member (car item) needed) #t #f)) secrets))))))))))
