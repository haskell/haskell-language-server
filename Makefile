UNAME := $(shell uname)
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

GHC_VERSION  ?=

HLS_VERSION := $(shell grep '^version:' haskell-language-server.cabal | awk '{ print $$2 }')

CHMOD    := chmod
INSTALL  := install
PATCHELF := patchelf
FIND     := find
SED      := sed
MKDIR    := mkdir
TAR      := tar
CABAL    := cabal
AWK      := awk
STRIP    := strip

INSTALL_NAME_TOOL := install_name_tool

STORE_DIR        := store
BINDIST_BASE_DIR := out/bindist
BINDIST_OUT_DIR  := $(BINDIST_BASE_DIR)/haskell-language-server-$(HLS_VERSION)

CABAL_ARGS         ?= --store-dir=$(ROOT_DIR)/$(STORE_DIR)
CABAL_INSTALL_ARGS ?= --disable-tests --disable-profiling -O2 --overwrite-policy=always --install-method=copy

hls: bindist/ghcs
	for ghc in $(shell cat bindist/ghcs) ; do \
		$(MAKE) GHC_VERSION=`echo $$ghc | $(AWK) -F ',' '{ print $$1 }'` PROJECT_FILE=`echo $$ghc | $(AWK) -F ',' '{ print $$2 }'` hls-ghc ; \
	done

hls-ghc:
	$(MKDIR) -p out/
	@if test -z "$(GHC_VERSION)" ; then echo >&2 "GHC_VERSION is not set" ; false ; fi
	@if test -z "$(PROJECT_FILE)" ; then echo >&2 "PROJECT_FILE is not set" ; false ; fi
	$(CABAL) $(CABAL_ARGS) v2-install --project-file=$(PROJECT_FILE) -w ghc-$(GHC_VERSION) $(CABAL_INSTALL_ARGS) --installdir=$(ROOT_DIR)/out/$(GHC_VERSION) exe:haskell-language-server exe:haskell-language-server-wrapper
ifeq ($(UNAME), Darwin)
	$(STRIP) $(ROOT_DIR)/out/$(GHC_VERSION)/haskell-language-server
	$(STRIP) $(ROOT_DIR)/out/$(GHC_VERSION)/haskell-language-server-wrapper
else
	$(STRIP) -s $(ROOT_DIR)/out/$(GHC_VERSION)/haskell-language-server
	$(STRIP) -s $(ROOT_DIR)/out/$(GHC_VERSION)/haskell-language-server-wrapper
endif

bindist:
	for ghc in $(shell cat bindist/ghcs) ; do \
		$(MAKE) GHC_VERSION=`echo $$ghc | $(AWK) -F ',' '{ print $$1 }'` bindist-ghc ; \
	done
	$(SED) -e "s/@@HLS_VERSION@@/$(HLS_VERSION)/" \
		bindist/Makefile.in > $(BINDIST_OUT_DIR)/Makefile
	$(INSTALL) -d $(BINDIST_OUT_DIR)/scripts/
	$(INSTALL) -vm 755 bindist/relpath.sh $(BINDIST_OUT_DIR)/scripts/relpath.sh

bindist-tar:
	cd $(BINDIST_BASE_DIR) ; $(TAR) caf $(ROOT_DIR)/out/haskell-language-server-$(HLS_VERSION).tar.xz haskell-language-server-$(HLS_VERSION)

bindist-ghc:
	if test -z "$(GHC_VERSION)" ; then echo >&2 "GHC_VERSION is not set" ; false ; fi
	$(MKDIR) -p $(BINDIST_OUT_DIR)/bin
	$(MKDIR) -p $(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)
	$(INSTALL) -d $(BINDIST_OUT_DIR)/bin/
	$(INSTALL) -vm 755 out/$(GHC_VERSION)/haskell-language-server $(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION)
ifeq ($(UNAME), Darwin)
	$(INSTALL_NAME_TOOL) -add_rpath "@executable_path/../lib/$(GHC_VERSION)" $(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION)
else
	$(PATCHELF) --set-rpath \$$ORIGIN/../lib/$(GHC_VERSION) $(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION)
endif
	$(SED) \
		-e "s/@@EXE_NAME@@/haskell-language-server-$(GHC_VERSION)/" \
		-e "s/@@GHC_VERSION@@/$(GHC_VERSION)/" \
		-e "s/@@ABI_HASHES@@/$(shell for dep in ghc `ghc-pkg-$(GHC_VERSION) field ghc depends --simple-output` ; do ghc-pkg-$(GHC_VERSION) field $$dep abi --simple-output ; done | tr '\n' ' ' | xargs)/" \
		bindist/wrapper.in > $(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION)
	$(CHMOD) 755 $(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION)
	$(INSTALL) -d $(BINDIST_OUT_DIR)/bin/
	$(INSTALL) -vm 755 out/$(GHC_VERSION)/haskell-language-server-wrapper $(BINDIST_OUT_DIR)/bin/haskell-language-server-wrapper
	$(FIND) $(STORE_DIR)/ghc-$(GHC_VERSION) -type f -name "*.so" -execdir install -vDm 755 {} $(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)/{} \;
	$(FIND) $(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION) -type f -name '*.so' -execdir $(PATCHELF) --set-rpath \$$ORIGIN {} \;

install-ghcs:
	ghcup install ghc recommended
	ghcup set ghc recommended
	for ghc in $(shell cat bindist/ghcs) ; do \
		ghcup install ghc `echo $$ghc | $(AWK) -F ',' '{ print $$1 }'` ; \
	done

version:
	@echo $(HLS_VERSION)


clean:
	rm -rf out/*

clean-all:
	rm -rf out/* $(STORE_DIR)

.PHONY: hls hls-ghc bindist bindist-ghc bindist-tar clean clean-all install-ghcs
