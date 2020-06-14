# Keep this short to avoid bloating beam files with long file path info
TOPDIR := /tmp/hamler
SRCDIR := $(TOPDIR)/$(PKG_VSN)
BUILT := $(SRCDIR)/BUILT
dash := -
none :=
space := $(none) $(none)

RPM_VSN ?= $(shell echo $(PKG_VSN) | grep -oE "[0-9]+\.[0-9]+(\.[0-9]+)?")
RPM_REL ?= $(shell echo $(PKG_VSN) | grep -oE "(alpha|beta|rc)\.[0-9]")

ifeq ($(RPM_REL),)
	# no tail
	RPM_REL := 1
endif

ARCH:=$(shell uname -m)
ifeq ($(ARCH),mips64)
	ARCH:=mips64el
endif

HAMLER_LIB=$(shell sed -r 's:^path (.*):/usr/lib/hamler:g' ../../../Env)

.PHONY: all
all: | $(BUILT)
	rpmbuild -v -bb \
		--define "_package_name hamler" \
		--define "_name hamler" \
		--define "_topdir $(TOPDIR)" \
		--define "_srcdir $(SRCDIR)" \
		--define "_version $(RPM_VSN)" \
		--define "_release $(RPM_REL)" \
		--define "_ostype -$(SYSTEM)" \
		hamler.spec
	mkdir -p _packages
	cp $(TOPDIR)/RPMS/$(ARCH)/*.rpm _packages/

$(BUILT):
	mkdir -p $(TOPDIR) $(SRCDIR)
	cp -r $(HAMLER_LIB)/bin $(SRCDIR)/
	cp -r $(HAMLER_LIB)/lib $(SRCDIR)/
	cp -r $(HAMLER_LIB)/ebin $(SRCDIR)/

clean:
	rm -rf $(SRCDIR)
