BASEDIR := ../..
HEADER  := $(BASEDIR)/gmqcc.h
MAJOR   := `sed -n -e '/GMQCC_VERSION_MAJOR/{s/.* .* //;p;q;}' $(HEADER)`
MINOR   := `sed -n -e '/GMQCC_VERSION_MINOR/{s/.* .* //;p;q;}' $(HEADER)`
PATCH   := `sed -n -e '/GMQCC_VERSION_PATCH/{s/.* .* //;p;q;}' $(HEADER)`
BINDIR  := gmqcc-$(MAJOR).$(MINOR).$(PATCH)

base:
	$(MAKE) CC=x86_64-w64-mingw32-gcc UNAME=MINGW -C $(BASEDIR) clean
	$(MAKE) CC=x86_64-w64-mingw32-gcc UNAME=MINGW -C $(BASEDIR) DESTDIR=distro/win64/$(BINDIR) PREFIX=/ strip install
	@mkdir -p $(BINDIR)/doc
	@groff -mandoc $(BINDIR)/man1/gmqpak.1 | ps2pdf - $(BINDIR)/doc/gmqpak.pdf
	@groff -mandoc $(BINDIR)/man1/qcvm.1 | ps2pdf - $(BINDIR)/doc/qcvm.pdf
	@groff -mandoc $(BINDIR)/man1/gmqcc.1 | ps2pdf - $(BINDIR)/doc/gmqcc.pdf
	@rm -rf $(BINDIR)/man1/
	@zip -r $(BINDIR)-win64.zip $(BINDIR)
	@rm -rf $(BINDIR)
clean:
	$(MAKE) -C $(BASEDIR) clean
	@rm -f *.zip

all: base
