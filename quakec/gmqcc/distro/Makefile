DROPBOX := dropbox_uploader.sh
UNAME   := $(shell uname -m)
DOWNLOAD:= ../doc/html/download.c
BRANCH  := $(shell git rev-parse --abbrev-ref HEAD)
ifneq ($(shell uname -m), x86_64)
    $(error Cannot build packages without an x86_64 capable CPU)
endif

.NOTPARALLEL: base
.NOTPARALLEL: upload

HEADER=\e[5;32;40m
RESET=\e[0;37;40m
INFO=\e[5;33;40m

base:
	@echo -e "\n$(HEADER)Building Debian packages ...$(RESET)"
	@echo -e "   $(INFO)=> building 64-bit package$(RESET)"
	@$(MAKE) -C deb/ >/dev/null
	@echo -e "   $(INFO)=> building 32-bit package$(RESET)"
	@$(MAKE) -C deb/ CARCH=i686 OPTIONAL_CFLAGS=-m32 OPTIONAL_LDFLAGS=-m32 >/dev/null
	
	@echo -e "\n$(HEADER)Building ArchLinux packages ...$(RESET)"
	@echo -e "   $(INFO)=> building 64-bit package$(RESET)"
	@$(MAKE) -C archlinux/this/ >/dev/null
	@echo -e "   $(INFO)=> building 32-bit package$(RESET)"
	@$(MAKE) -C archlinux/this/ CARCH=i686 OPTIONAL_CFLAGS=-m32 OPTIONAL_LDFLAGS=-m32 >/dev/null

	@echo -e "\n$(HEADER)Building ArchBSD packages ...$(RESET)"
	@echo -e "   $(INFO)=> building 64-bit package$(RESET)"
	@$(MAKE) -C archbsd/this/ >/dev/null
	@echo -e "   $(INFO)=> building 32-bit package$(RESET)"
	@$(MAKE) -C archbsd/this/ CARCH=i686 OPTIONAL_CFLAGS=-m32 OPTIONAL_LDFLAGS=-m32 >/dev/null
	
	@echo -e "\n$(HEADER)Building Slackware packages ...$(RESET)"
	@echo -e "   $(INFO)=> building 64-bit package$(RESET)"
	@$(MAKE) -C slackware/this/ >/dev/null
	@echo -e "   $(INFO)=> building 32-bit package$(RESET)"
	@$(MAKE) -C slackware/this/ CARCH=i686 OPTIONAL_CFLAGS=-m32 OPTIONAL_LDFLAGS=-m32 >/dev/null

	@echo -e "\n$(HEADER)Building Fedora packages ...$(RESET)"
	@echo -e "   $(INFO)=> building 64-bit package$(RESET)"
	@$(MAKE) -C fedora/this/ >/dev/null

	@echo -e "\n\$(HEADER)Building Windows packages ...$(RESET)"
	@echo -e "   $(INFO)=> building 64-bit package$(RESET)"
	@$(MAKE) -C win64/ >/dev/null
	@echo -e "   $(INFO)=> building 32-bit package$(RESET)"
	@$(MAKE) -C win32/ >/dev/null
	
	@rm -rf pkgs/
	@mkdir pkgs/
	@mv deb/*.deb ./pkgs/
	@mv archlinux/this/*pkg.tar.xz ./pkgs/
	@mv archbsd/this/*pkg.tar.xz ./pkgs/
	@mv win32/*.zip ./pkgs/
	@mv win64/*.zip ./pkgs/
	@mv slackware/this/*.txz ./pkgs/
	@mv fedora/this/*.rpm ./pkgs/
	
	@echo -e "\n\n$(HEADER)Completed:$(RESET)"
	@find ./pkgs/ -type f -regex ".*/.*\.\(xz\|deb\|zip\|txz\|rpm\)" -exec echo -e "  $(INFO)=>$(RESET) {}" \;
	
upload:
	@echo "APPKEY:76vh3q42hnvmzm3" > dropbox_config
	@echo "APPSECRET:tmeecht2cmh72xa" >> dropbox_config
	@echo "ACCESS_LEVEL:sandbox" >> dropbox_config
	@echo "OAUTH_ACCESS_TOKEN:w0bxzf0dft8edfq" >> dropbox_config
	@echo "OAUTH_ACCESS_TOKEN_SECRET:9vosx7x8gy4kgjk" >> dropbox_config
	@wget -q "http://raw.github.com/andreafabrizi/Dropbox-Uploader/master/dropbox_uploader.sh"
	@chmod +x dropbox_uploader.sh
	@sed -i -e "s/~\/.dropbox_uploader/.\/dropbox_config/g" $$(basename $(DROPBOX))
	@find ./pkgs -type f -regex ".*/.*\.\(xz\|deb\|zip\|txz\|rpm\)" -exec ./$$(basename $(DROPBOX)) upload {} \;
	@rm dropbox_config dropbox_uploader.sh

website:
	$(CC) $(DOWNLOAD) -o html.gen
	@./html.gen ../
	@rm html.gen
	@git stash
	@git checkout gh-pages
	@rm -f ../download.html
	@mv -f download.html ../download.html
	@cd ..; git add download.html; git commit -m 'update download page'; git push origin gh-pages;
	@git checkout $(BRANCH)
	@git stash apply

clean:
	@rm -rf pkgs/
	@rm -f *.html

all: base upload
