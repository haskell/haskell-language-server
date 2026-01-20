####################################################
# This makefile's main purpose is to build
# dynamically linked HLS executables on gitlab CI
# and produce appropriate bindists. This can also
# be executed locally on dev machines.
#
# It is not meant to be run by users.
# ##################################################

UNAME := $(shell uname)
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

GHC_VERSION  ?=
ARTIFACT     ?= unknown-platform

HLS_VERSION := $(shell grep '^version:' haskell-language-server.cabal | awk '{ print $$2 }')
TARBALL     ?= haskell-language-server-$(HLS_VERSION)-$(ARTIFACT).tar.xz

CHMOD     := chmod
CHMOD_X   := $(CHMOD) 755
INSTALL   := install
INSTALL_D := $(INSTALL) -d
INSTALL_X := $(INSTALL) -vm 755
PATCHELF  := patchelf
FIND      := find
SED       := sed
MKDIR     := mkdir
MKDIR_P   := $(MKDIR) -p
TAR       := tar
TAR_MK    := $(TAR) caf
CABAL     := cabal
STRIP     := strip
ifeq ($(UNAME), Darwin)
STRIP_S   := strip
else
STRIP_S   := strip -s
endif
RM        := rm
RM_RF     := $(RM) -rf
CD        := cd
CP        := cp

# by default don't run ghcup
GHCUP       ?= echo
GHCUP_GC    ?= $(GHCUP) gc
GHCUP_RM    ?= $(GHCUP) rm

CABAL_CACHE_BIN ?= echo

ifeq ($(UNAME), Darwin)
DLL       := *.dylib
else
DLL       := *.so
endif

INSTALL_NAME_TOOL := install_name_tool

STORE_DIR        := store/$(ARTIFACT)
BINDIST_BASE_DIR := out/bindist/$(ARTIFACT)
BINDIST_OUT_DIR  := $(BINDIST_BASE_DIR)/haskell-language-server-$(HLS_VERSION)

CABAL_BASE_ARGS         ?= --store-dir=$(ROOT_DIR)/$(STORE_DIR)
CABAL_ARGS              ?= --disable-tests --disable-profiling -O2 $(ADD_CABAL_ARGS)
CABAL_INSTALL_ARGS      ?= --overwrite-policy=always --install-method=copy
CABAL_INSTALL           := $(CABAL) $(CABAL_BASE_ARGS) v2-install
PROJECT_FILE            := cabal.project

S3_HOST ?=
S3_KEY  ?=

# set rpath relative to the current executable
# TODO: on darwin, this doesn't overwrite rpath, but just adds to it,
#       so we'll have the old rpaths from the build host in there as well
define set_rpath
	$(if $(filter Darwin,$(UNAME)), $(INSTALL_NAME_TOOL) -add_rpath "@executable_path/$(1)" "$(2)", $(PATCHELF) --force-rpath --set-rpath "\$$ORIGIN/$(1)" "$(2)")
endef

define sync_from
	$(CABAL_CACHE_BIN) sync-from-archive --host-name-override=$(S3_HOST) --host-port-override=443 --host-ssl-override=True --region us-west-2 --store-path="$(ROOT_DIR)/$(STORE_DIR)" --archive-uri "s3://haskell-language-server/$(S3_KEY)"
endef

define sync_to
	$(CABAL_CACHE_BIN) sync-to-archive --host-name-override=$(S3_HOST) --host-port-override=443 --host-ssl-override=True --region us-west-2 --store-path="$(ROOT_DIR)/$(STORE_DIR)" --archive-uri "s3://haskell-language-server/$(S3_KEY)"
endef

hls:
	@if test -z "$(GHCS)" ; then echo >&2 "GHCS is not set" ; false ; fi
	for ghc in $(GHCS) ; do \
		$(GHCUP) install ghc `echo $$ghc` && \
		$(GHCUP_GC) -p -s -c -t && \
		$(MAKE) GHC_VERSION=`echo $$ghc` hls-ghc || exit 1 && \
		$(GHCUP_RM) `echo $$ghc` ; \
	done

hls-ghc:
	$(MKDIR_P) out/$(ARTIFACT)
	$(MKDIR_P) out/plan.json
	@if test -z "$(GHC_VERSION)" ; then echo >&2 "GHC_VERSION is not set" ; false ; fi
	$(CABAL) $(CABAL_BASE_ARGS) configure --project-file="$(PROJECT_FILE)" -w "ghc-$(GHC_VERSION)" $(CABAL_ARGS) exe:haskell-language-server exe:haskell-language-server-wrapper
	$(CABAL) $(CABAL_BASE_ARGS) build --project-file="$(PROJECT_FILE)" -w "ghc-$(GHC_VERSION)" $(CABAL_ARGS) --dependencies-only --dry-run exe:haskell-language-server exe:haskell-language-server-wrapper
	$(call sync_from)
	$(CP) dist-newstyle/cache/plan.json "$(ROOT_DIR)/out/plan.json/$(ARTIFACT)-ghc-$(GHC_VERSION)-plan.json"
	$(CABAL_INSTALL) --project-file="$(PROJECT_FILE)" -w "ghc-$(GHC_VERSION)" $(CABAL_ARGS) $(CABAL_INSTALL_ARGS) --installdir="$(ROOT_DIR)/out/$(ARTIFACT)/$(GHC_VERSION)" exe:haskell-language-server exe:haskell-language-server-wrapper
	$(call sync_to)
	$(STRIP_S) "$(ROOT_DIR)/out/$(ARTIFACT)/$(GHC_VERSION)/haskell-language-server"
	$(STRIP_S) "$(ROOT_DIR)/out/$(ARTIFACT)/$(GHC_VERSION)/haskell-language-server-wrapper"

bindist:
	@if test -z "$(GHCS)" ; then echo >&2 "GHCS is not set" ; false ; fi
	for ghc in $(GHCS) ; do \
		$(GHCUP) install ghc `echo $$ghc` && \
		$(GHCUP_GC) -p -s -c -t && \
		$(MAKE) GHC_VERSION=`echo $$ghc` bindist-ghc || exit 1 && \
		$(GHCUP_RM) ghc `echo $$ghc` ; \
	done
	$(SED) -e "s/@@HLS_VERSION@@/$(HLS_VERSION)/" \
		bindist/GNUmakefile.in > "$(BINDIST_OUT_DIR)/GNUmakefile"
	$(INSTALL_D) "$(BINDIST_OUT_DIR)/scripts/"
	$(INSTALL_X) "bindist/relpath.sh" "$(BINDIST_OUT_DIR)/scripts/relpath.sh"

bindist-tar:
	$(CD) "$(BINDIST_BASE_DIR)" ; $(TAR_MK) "$(ROOT_DIR)/out/$(TARBALL)" "haskell-language-server-$(HLS_VERSION)"

bindist-ghc:
	if test -z "$(GHC_VERSION)" ; then echo >&2 "GHC_VERSION is not set" ; false ; fi
	$(MKDIR_P) "$(BINDIST_OUT_DIR)/bin"
	$(MKDIR_P) "$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)"
	$(INSTALL_D) "$(BINDIST_OUT_DIR)/bin/"
	$(INSTALL_X) "out/$(ARTIFACT)/$(GHC_VERSION)/haskell-language-server" "$(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION)"
	$(call set_rpath,../lib/$(GHC_VERSION),$(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION))
	$(SED) \
		-e "s/@@EXE_NAME@@/haskell-language-server-$(GHC_VERSION)/" \
		-e "s/@@GHC_VERSION@@/$(GHC_VERSION)/" \
		-e "s/@@BOOT_PKGS@@/$(shell ghc-pkg-$(GHC_VERSION) --global list --simple-output)/" \
		-e "s/@@ABI_HASHES@@/$(shell for dep in `ghc-pkg-$(GHC_VERSION) --global list --simple-output` ; do printf "%s:" "$$dep" && ghc-pkg-$(GHC_VERSION) field $$dep abi --simple-output ; done | tr '\n' ' ' | xargs)/" \
		bindist/wrapper.in > "$(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION).in"
	$(CHMOD_X) "$(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION).in"
	$(INSTALL_D) "$(BINDIST_OUT_DIR)/bin/"
	$(INSTALL_X) "out/$(ARTIFACT)/$(GHC_VERSION)/haskell-language-server-wrapper" "$(BINDIST_OUT_DIR)/bin/haskell-language-server-wrapper"
	$(INSTALL_D) "$(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)"
# with newer cabal, the store path contains a hash for the ghc ABI, so paths look like store/ghc-9.10.3-abcdef
	$(FIND) $(wildcard $(STORE_DIR)/ghc-$(GHC_VERSION)*) -type f -name "$(DLL)" -execdir $(INSTALL_X) "{}" "$(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)/{}" \;
	$(FIND) "$(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)" -type f -name '$(DLL)' -execdir $(call set_rpath,,{}) \;

version:
	@echo "$(HLS_VERSION)"

clean:
	$(RM_RF) out/*

clean-ghcs:
	@if test -z "$(GHCS)" ; then echo >&2 "GHCS is not set" ; false ; fi
	for ghc in $(GHCS) ; do \
		$(GHCUP) rm ghc `echo $$ghc` ; \
	done

clean-all: clean-ghcs
	$(RM_RF) out/* $(STORE_DIR)

.PHONY: hls hls-ghc bindist bindist-ghc bindist-tar clean clean-all install-ghcs version
