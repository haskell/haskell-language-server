ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

GHC_VERSION  ?=
HLS_VERSION  ?=

ifndef HLS_VERSION
$(error HLS_VERSION is not set)
endif

CHMOD    := chmod
INSTALL  := install
PATCHELF := patchelf
FIND     := find
SED      := sed
MKDIR    := mkdir
TAR      := tar
CABAL    := cabal
AWK      := awk

STORE_DIR        := store
BINDIST_BASE_DIR := out/bindist
BINDIST_OUT_DIR  := $(BINDIST_BASE_DIR)/haskell-language-server-$(HLS_VERSION)

CABAL_ARGS         ?= --store-dir=$(ROOT_DIR)/$(STORE_DIR)
CABAL_INSTALL_ARGS ?= --overwrite-policy=always --install-method=copy

hls: bindist/ghcs
	for ghc in $(shell cat bindist/ghcs) ; do \
		$(MAKE) GHC_VERSION=`echo $$ghc | $(AWK) -F ',' '{ print $$1 }'` PROJECT_FILE=`echo $$ghc | $(AWK) -F ',' '{ print $$2 }'` hls-ghc ; \
	done

hls-ghc:
	@if test -z "$(GHC_VERSION)" ; then echo >&2 "GHC_VERSION is not set" ; false ; fi
	@if test -z "$(PROJECT_FILE)" ; then echo >&2 "PROJECT_FILE is not set" ; false ; fi
	$(CABAL) $(CABAL_ARGS) v2-install --project-file=$(PROJECT_FILE) -w ghc-$(GHC_VERSION) $(CABAL_INSTALL_ARGS) --installdir=$(ROOT_DIR)/out/$(GHC_VERSION)

bindist:
	for ghc in $(shell cat bindist/ghcs) ; do \
		$(MAKE) GHC_VERSION=`echo $$ghc | $(AWK) -F ',' '{ print $$1 }'` bindist-ghc ; \
	done
	$(SED) -e "s/@@HLS_VERSION@@/$(HLS_VERSION)/" \
		bindist/Makefile.in > $(BINDIST_OUT_DIR)/Makefile
	cd $(BINDIST_BASE_DIR) ; $(TAR) caf $(ROOT_DIR)/out/haskell-language-server-$(HLS_VERSION).tar.xz haskell-language-server-$(HLS_VERSION)

bindist-ghc:
	if test -z "$(GHC_VERSION)" ; then echo >&2 "GHC_VERSION is not set" ; false ; fi
	$(MKDIR) -p $(BINDIST_OUT_DIR)/bin
	$(MKDIR) -p $(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)
	$(SED) -e "s/@@EXE_NAME@@/haskell-language-server-$(GHC_VERSION)/" -e "s/@@GHC_VERSION@@/$(GHC_VERSION)/" \
		bindist/wrapper.in > $(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION)
	$(SED) -e 's/@@EXE_NAME@@/haskell-language-server-wrapper/' -e "s/@@GHC_VERSION@@/$(GHC_VERSION)/" \
		bindist/wrapper.in > $(BINDIST_OUT_DIR)/haskell-language-server-wrapper
	$(CHMOD) 755 $(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION)
	$(CHMOD) 755 $(BINDIST_OUT_DIR)/haskell-language-server-wrapper
	$(INSTALL) -vDm 755 out/$(GHC_VERSION)/haskell-language-server $(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION)
	$(PATCHELF) --set-rpath \$$ORIGIN/../lib/$(GHC_VERSION) $(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION)
	$(INSTALL) -vDm 755 out/$(GHC_VERSION)/haskell-language-server-wrapper $(BINDIST_OUT_DIR)/bin/haskell-language-server-wrapper
	$(PATCHELF) --set-rpath \$$ORIGIN/../lib/$(GHC_VERSION) $(BINDIST_OUT_DIR)/bin/haskell-language-server-wrapper
	$(FIND) $(STORE_DIR)/ghc-$(GHC_VERSION) -type f -name "*.so" -execdir install -vDm 755 {} $(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)/{} \;
	$(FIND) $(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION) -type f -name '*.so' -execdir $(PATCHELF) --set-rpath \$$ORIGIN {} \;


clean:
	rm -rf $(BINDIST_BASE_DIR)

clean-all:
	rm -rf $(BINDIST_BASE_DIR) $(STORE_DIR)

.PHONY: hls hls-ghc bindist bindist-ghc clean clean-all
