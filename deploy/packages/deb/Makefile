# Keep this short to avoid bloating beam files with long file path info
TOPDIR := /tmp/hamler
SRCDIR := $(TOPDIR)/$(PKG_VSN)
BUILT := $(SRCDIR)/BUILT

HAMLER_LIB=$(shell sed -r 's:^path (.*):/usr/lib/hamler:g' ../../../Env)

.PHONY: all
all: | $(BUILT)
	cp -r debian $(SRCDIR)/
	sed -i "s#<DATETIME>#$(shell date -u '+%a, %d %b %Y %T %z')#g" $(SRCDIR)/debian/changelog
	sed -i "s#<VERSION>#$(PKG_VSN)#g" $(SRCDIR)/debian/changelog
	cd $(SRCDIR) && dpkg-buildpackage -us -uc
	mkdir _packages
	cp $(TOPDIR)/$(SOURCE_PKG)/*.deb _packages/

$(BUILT):
	mkdir -p $(TOPDIR) $(SRCDIR)
	cp -r $(HAMLER_LIB)/bin $(SRCDIR)/
	cp -r $(HAMLER_LIB)/lib $(SRCDIR)/
	cp -r $(HAMLER_LIB)/ebin $(SRCDIR)

clean:
	rm -rf $(SRCDIR)
