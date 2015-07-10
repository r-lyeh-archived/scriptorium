BASEDIR := $(CURDIR)/../../..
HEADER  := $(BASEDIR)/gmqcc.h
MAJOR   := `sed -n -e '/GMQCC_VERSION_MAJOR/{s/.* .* //;p;q;}' $(HEADER)`
MINOR   := `sed -n -e '/GMQCC_VERSION_MINOR/{s/.* .* //;p;q;}' $(HEADER)`
PATCH   := `sed -n -e '/GMQCC_VERSION_PATCH/{s/.* .* //;p;q;}' $(HEADER)`
NAME    := gmqcc-$(MAJOR).$(MINOR).$(PATCH)
TARFILE := $(NAME).tar.gz

all:
	@mkdir -p ~/rpmbuild/SPECS
	@mkdir -p ~/rpmbuild/SOURCES
	@cp ../spec/gmqcc.spec ~/rpmbuild/SPECS
	@mkdir -p /tmp/$(NAME)
	@cp -R $(BASEDIR) /tmp/$(NAME)/
	@cd /tmp && tar -zcf ~/rpmbuild/SOURCES/$(TARFILE) $(NAME)/
	@rm -rf /tmp/$(NAME)
	@rpmbuild -ba ../spec/gmqcc.spec 2>&1 >/dev/null
	
	@mv ~/rpmbuild/RPMS/x86_64/gmqcc*.rpm . 2>/dev/null; true
	@mv ~/rpmbuild/RPMS/x86_64/qcvm*.rpm . 2>/dev/null; true
	@mv ~/rpmbuild/RPMS/x86_64/gmqpak*.rpm . 2>/dev/null; true
