export

ifneq ($(shell uname -s),Linux)
	$(shell echo "Please execute this script under Linux")
	exit
endif

# The version-release used for package
PKG_VSN := $(shell git describe --tags --always)

##
## Support RPM and Debian based linux systems
##
ifneq ($(shell cat /etc/*-release |grep -o -i centos),)
	PKGERDIR := rpm
else
	PKGERDIR := deb
endif

.PHONY: all
all: clean
	$(if $(PKGERDIR),,$(error "Operating system '$(OS)' not supported"))
	cd $(PKGERDIR) && $(MAKE)

clean:
	make -C rpm clean
	make -C deb clean

.PHONY: deb
deb:
	make -C deb

.PHONY: rpm
rpm:
	make -C rpm

