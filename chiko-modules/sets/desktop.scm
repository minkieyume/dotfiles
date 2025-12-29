;; -*- mode: scheme -*-
;; SPDX-FileCopyrightText: 2023, 2024 Minkie Yume <minkieyume@yumieko.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (chiko-modules sets desktop)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services fontutils)
  #:use-module (rosenthal)
  #:use-module (chiko-modules utils)
  #:use-module (chiko-modules loader dir-loader)
  #:use-module (chiko-modules sets)
  #:use-module (chiko-modules packages desktop)
  #:export (make-wayland-desktop
	    make-fcitx5
	    make-fontconfig
	    make-mako
	    make-rofi
	    make-swaybg
	    make-niri
	    make-waybar
	    make-greetd-transform))

(define (make-fontconfig)
  (simple-service 'extra-fontconfig
		  home-fontconfig-service-type
		  (let ((sans  "SF Pro Text")
			(serif "New York Medium")
			(mono  "Victor Mono")
			(emoji "Noto Color Emoji"))
		    `((alias
		       (family "sans-serif")
		       (prefer
			(family ,sans)
			(family "Noto Sans CJK SC")
			(family ,emoji)))
		      (alias
		       (family "serif")
		       (prefer
			(family ,serif)
			(family "Noto Serif CJK SC")
			(family ,emoji)))
		      (alias
		       (family "monospace")
		       (prefer
			(family ,mono)
			(family "Sarasa Mono SC")
			(family ,emoji)))

		      ,@(map (lambda (name)
			       `(alias
				 (family ,name)
				 (prefer
				  (family ,sans)
				  (family "sans-serif"))))
			     '("system-ui"
			       "ui-sans-serif"))
		      (alias
		       (family "ui-serif")
		       (prefer
			(family ,serif)
			(family "serif")))
		      (alias
		       (family "ui-monospace")
		       (prefer
			(family ,mono)
			(family "monospace")))))))

(define (make-mako machine-name)
  (service home-mako-service-type
	   (home-mako-configuration
	    (config
	     (local-file (string-append %configdir
					machine-name
					"/mako.conf"))))))

(define (make-rofi machine-name)
  (service home-rofi-service-type
  	 (home-rofi-configuration
  	  (config
  	   (mixed-text-file "rofi.rasi" "\
  configuration {
      icon-theme: \"Qogir\";
  }
  @theme \"" (spec->pkg "rofi") "/share/rofi/themes/fullscreen-preview.rasi\"\n")))))

(define (make-niri machine-name wallpapers niri-pkgs)
  (service home-niri-service-type
  	 (home-niri-configuration
  	  (config
  	   (computed-substitution-with-inputs "niri.kdl"
  					      (local-file (string-append %configdir machine-name "/niri.kdl"))
  					      (cons* (local-file wallpapers #:recursive? #t)
						       (apply specs->pkgs niri-pkgs)))))))

(define (make-waybar machine-name)
  (service home-waybar-service-type
	   (home-waybar-configuration
	    (config
             (computed-substitution-with-inputs "config.json"
						(local-file
						 (string-append
						  %configdir
						  machine-name
						  "/waybar.json"))
						(specs->pkgs "light" "wireplumber" "rofi" "pavucontrol")))
	    (style
		(local-file
		 (string-append
		  %configdir
		  machine-name
		  "/waybar.css"))))))

(define (make-fcitx5 themes input-methods)
  (cfgset
   (home-settings
    `((services ,(list
		  (service home-fcitx5-service-type
  			   (home-fcitx5-configuration
  			    (themes (apply specs->pkgs themes))
  			    (input-method-editors
			     (apply specs->pkgs input-methods))
  			    (qt-im-module? #t)))))))
   (home-envs
    `(("SDL_IM_MODULE" . "fcitx")
      ("GLFW_IM_MODULE" . "ibus")
      ("QT_IM_MODULES" . "wayland;fcitx;ibus")
      ("GTK_IM_MODULE_FILE" . "$GUIX_GTK3_IM_MODULE_FILE")))))

(define (make-swaybg machine-name background)
  (service home-swaybg-service-type
  	 (home-swaybg-configuration
  	  (background background))))

(define (make-greetd-transform)
  (lambda (os)
    (operating-system
     (inherit os)
     (services (modify-services (operating-system-user-services os)
  				(greetd-service-type
  				 config => (greetd-configuration
  					    (inherit config)
  					    (terminals
  					     (map (lambda (x)
  						    (greetd-terminal-configuration
  						     (terminal-vt (number->string x))
  						     (terminal-switch (eqv? 1 x))
  						     (default-session-command
  						       (cond
  							((eqv? 1 x)
  							 (greetd-tuigreet-session))
  							(else
  							 (greetd-agreety-session
  							  (command
  							   (greetd-user-session
  							    (command #~(getenv "SHELL"))))))))))
  						  (iota 6 1))))))))))


(define* (make-wayland-desktop machine-name			       
			       #:key
			       (composer (make-niri machine-name
						    (string-append %secretdir "wallpapers")
						    '("foot"
                                                      "light"
                                                      "rofi"
                                                      "swaylock-effects"
                                                      "wireplumber"
                                                      "xwayland-satellite")))
			       (fontconfig
				(make-fontconfig))
			       (input-method
				(make-fcitx5
				 '("fcitx5-material-color-theme")
				 '("fcitx5-rime" "fcitx5-anthy")))
			       (notification
				(make-mako machine-name))
			       (launcher
				(make-rofi machine-name))
			       (bar
				(make-waybar machine-name))
			       (background
				(make-swaybg machine-name
					     (local-file (string-append
							  %secretdir "wallpapers/yumemi_wallpaper1.png"))))
			       (sys-desktop-services
				%rosenthal-desktop-services)
			       (home-desktop-services
				%rosenthal-desktop-home-services))
  (merge-sets
   input-method
   (cfgset
    (sys-transforms
     (list (make-greetd-transform)))
    (sys-settings
     `((services
	,sys-desktop-services)
       (packages ,%desktop-packages)))
    (home-files `((".gtkrc-2.0"
  		   ,(local-file
		     (string-append
		      %configdir
		      machine-name
		      "/gtk2.conf")))
		  ("gtk-3.0/settings.ini"
  		   ,(local-file
		     (string-append
		      %configdir
		      machine-name
		      "/gtk.conf")))
  		  ("gtk-4.0/settings.ini"
  		   ,(local-file
		     (string-append
		      %configdir
		      machine-name
		      "/gtk.conf")))))
    (home-configs `(("foot/foot.ini"
  		     ,(local-file (string-append
				   %configdir
				   machine-name "/foot.ini")))))
    (home-settings
     `((services ,(append (list composer fontconfig notification
				launcher bar background) home-desktop-services)))))))
