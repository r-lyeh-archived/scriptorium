BASEDIR := ../..
PREFIX  := /usr
HEADER  := $(BASEDIR)/gmqcc.h
MAJOR   := `sed -n -e '/GMQCC_VERSION_MAJOR/{s/.* .* //;p;q;}' $(HEADER)`
MINOR   := `sed -n -e '/GMQCC_VERSION_MINOR/{s/.* .* //;p;q;}' $(HEADER)`
PATCH   := `sed -n -e '/GMQCC_VERSION_PATCH/{s/.* .* //;p;q;}' $(HEADER)`
DEBDIR  := gmqcc-$(MAJOR).$(MINOR).$(PATCH)
CARCH   := $(shell uname -m)
DEB     := $(DEBDIR)-$(CARCH).deb
CONTROL := $(DEBDIR)/DEBIAN/control

ifneq (, $(findstring i686, $(CARCH)))
	CFLAGS += -m32
	LDFLAGS += -m32
endif

base:
	$(MAKE) -C $(BASEDIR) clean
	CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)" \
	  $(MAKE) -C $(BASEDIR) DESTDIR=distro/deb/$(DEBDIR) PREFIX=$(PREFIX) strip install
	@install -d -m755 $(DEBDIR)/DEBIAN
	@echo "Package: gmqcc" > $(CONTROL)
	@echo "Version: $(MAJOR).$(MINOR).$(PATCH)" >> $(CONTROL)
	@echo "Section: user/hidden" >> $(CONTROL)
	@echo "Priority: optional" >> $(CONTROL)
	@echo "Architecture: $(CARCH)" >> $(CONTROL)
	@echo "Installed-Size: `du -ks $($(DEBDIR)/usr) | cut -f 1`" >> $(CONTROL)
	@echo "Maintainer: Dale Weiler <killfieldengine@gmail.com>" >> $(CONTROL)
	@echo "Description: An improved Quake C Compiler" >> $(CONTROL)
	@echo "   For an enduring period of time the options for a decent compiler for the Quake C programming language" >> $(CONTROL)
	@echo "   were confined to a specific compiler known as QCC. Attempts were made to extend and improve upon the" >> $(CONTROL)
	@echo "   design of QCC, but many foreseen the consequences of building on a broken foundation. The solution" >> $(CONTROL)
	@echo "   was obvious, a new compiler; one born from the NIH realm of sarcastic wit. We welcome you. You won't" >> $(CONTROL)
	@echo "   find a better Quake C compiler." >> $(CONTROL)
	@tar czf data.tar.gz -C $(DEBDIR)/ . --exclude=DEBIAN
	@tar czf control.tar.gz -C $(DEBDIR)/DEBIAN/ .
	@echo 2.0 > debian-binary
	@ar r $(DEB) debian-binary control.tar.gz data.tar.gz 2>&1 >/dev/null
	@rm -rf debian-binary control.tar.gz data.tar.gz $(DEBDIR)

clean:
	$(MAKE) -C $(BASEDIR) clean
	@rm -f *.deb

all: base
