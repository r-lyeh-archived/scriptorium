BASEDIR := ../../../
PREFIX  := /usr
HEADER  := $(BASEDIR)/gmqcc.h
MAJOR   := $(shell sed -n -e '/GMQCC_VERSION_MAJOR/{s/.* .* //;p;q;}' $(HEADER))
MINOR   := $(shell sed -n -e '/GMQCC_VERSION_MINOR/{s/.* .* //;p;q;}' $(HEADER))
PATCH   := $(shell sed -n -e '/GMQCC_VERSION_PATCH/{s/.* .* //;p;q;}' $(HEADER))
PKGREL  := 1
SUFFIX  ?= archlinux
CARCH   := $(shell uname -m)
PKGDIR  := gmqcc-$(MAJOR).$(MINOR).$(PATCH)-$(PKGREL)-$(CARCH)-$(SUFFIX)
TARCOMP := -J
PKG     := $(PKGDIR).pkg.tar.xz
PKGINFO := $(PKGDIR)/.PKGINFO
DESTDIR := distro/archlinux/this
CFLAGS  :=

LIBC_DEPEND := glibc

base:
	$(MAKE) -C $(BASEDIR) clean
	CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)" \
	  $(MAKE) -C $(BASEDIR) "DESTDIR=$(DESTDIR)/$(PKGDIR)" "PREFIX=$(PREFIX)" strip install
	@echo "pkgname = gmqcc" > $(PKGINFO)
	@echo "pkgver = $(MAJOR).$(MINOR).$(PATCH)-$(PKGREL)" >> $(PKGINFO)
	@echo "pkgdesc = An Improved Quake C Compiler" >> $(PKGINFO)
	@echo "url = https://github.com/graphitemaster/gmqcc.git" >> $(PKGINFO)
	@echo "builddate = `date -u \"+%s\"`" >> $(PKGINFO)
	@echo "packager = Unknown Packager" >> $(PKGINFO)
	@echo "size = `du -sk $(PKGDIR) | awk '{print $$1 * 1024}'`" >> $(PKGINFO)
	@echo "arch = $(CARCH)" >> $(PKGINFO)
	@echo "license = MIT" >> $(PKGINFO)
	@echo "conflict = gmqcc" >> $(PKGINFO)
	@echo "depend = $(LIBC_DEPEND)" >> $(PKGINFO)
	@echo "makepkgopt = strip" >> $(PKGINFO)
	@echo "makepkgopt = docs" >> $(PKGINFO)
	@echo "makepkgopt = libtool" >> $(PKGINFO)
	@echo "makepkgopt = emptydirs" >> $(PKGINFO)
	@echo "makepkgopt = zipman" >> $(PKGINFO)
	@echo "makepkgopt = purge" >> $(PKGINFO)
	@echo "makepkgopt = !upx" >> $(PKGINFO)
	@bsdtar -C $(PKGDIR) -czf $(PKGDIR)/.MTREE \
	    --format=mtree \
	    --options='!all,use-set,type,uid,gid,mode,time,size,md5,sha256,link' \
	    .PKGINFO usr/ 2>&1 >/dev/null
	@bsdtar $(TARCOMP) -cvf $(PKG) -C $(PKGDIR)/ .PKGINFO .MTREE usr/ 2>&1 >/dev/null
	@rm -rf $(PKGDIR)

clean:
	$(MAKE) -C $(BASEDIR) clean
	@rm -f *.pkg.tar.xz

all: base
