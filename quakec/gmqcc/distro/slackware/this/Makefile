BASEDIR := ../../../
PREFIX  := /usr
HEADER  := $(BASEDIR)/gmqcc.h
MAJOR   := $(shell sed -n -e '/GMQCC_VERSION_MAJOR/{s/.* .* //;p;q;}' $(HEADER))
MINOR   := $(shell sed -n -e '/GMQCC_VERSION_MINOR/{s/.* .* //;p;q;}' $(HEADER))
PATCH   := $(shell sed -n -e '/GMQCC_VERSION_PATCH/{s/.* .* //;p;q;}' $(HEADER))
CARCH   := $(shell uname -m)
PKGDIR  := gmqcc-$(MAJOR).$(MINOR).$(PATCH)-$(CARCH)
PKG     := $(PKGDIR).txz
PKGINFO := $(PKGDIR)/.PKGINFO
DESTDIR := distro/slackware/this/$(PKGDIR)
CFLAGS  :=


ifneq (, $(findstring i686, $(CARCH)))
	CFLAGS += -m32
	LDFLAGS += -m32
endif

base:
	$(MAKE) -C $(BASEDIR) clean
	CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)" \
	  $(MAKE) -C $(BASEDIR) "DESTDIR=$(DESTDIR)" "PREFIX=$(PREFIX)" strip install
	gzip -9 $(PKGDIR)/usr/share/man/man?/*.?
	strip -s $(PKGDIR)/usr/bin/*
	mkdir $(PKGDIR)/install
	cp slack-desc $(PKGDIR)/install
	@tar -cJvf $(PKG) -C $(PKGDIR)/ install/ usr/
	@rm -rf $(PKGDIR)

clean:
	$(MAKE) -C $(BASEDIR) clean
	@rm -f *.txz

all: base
