build-%:
	doas guix system build build/build-$*.scm

reconfigure-%:
	doas guix system reconfigure build/build-$*.scm

deploy-%:
	guix deploy deploy/chikopara-deploy-$*.scm
