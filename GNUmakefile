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
TARBALL_ARCHIVE_SUFFIX ?=

HLS_VERSION := $(shell grep '^version:' haskell-language-server.cabal | awk '{ print $$2 }')
TARBALL     ?= haskell-language-server-$(HLS_VERSION)-$(TARBALL_ARCHIVE_SUFFIX).tar.xz

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
AWK       := awk
STRIP     := strip
ifeq ($(UNAME), Darwin)
STRIP_S   := strip
else
STRIP_S   := strip -s
endif
RM        := rm
RM_RF     := $(RM) -rf
CD        := cd

# by default don't run ghcup
GHCUP     ?= echo

ifeq ($(UNAME), Darwin)
DLL       := *.dylib
else
DLL       := *.so
endif

INSTALL_NAME_TOOL := install_name_tool

STORE_DIR        := store
BINDIST_BASE_DIR := out/bindist
BINDIST_OUT_DIR  := $(BINDIST_BASE_DIR)/haskell-language-server-$(HLS_VERSION)

CABAL_ARGS         ?= --store-dir=$(ROOT_DIR)/$(STORE_DIR)
CABAL_INSTALL_ARGS ?= --disable-tests --disable-profiling -O2 --overwrite-policy=always --install-method=copy
CABAL_INSTALL      := $(CABAL) $(CABAL_ARGS) v2-install

# set rpath relative to the current executable
# TODO: on darwin, this doesn't overwrite rpath, but just adds to it,
#       so we'll have the old rpaths from the build host in there as well
define set_rpath
	$(if $(filter Darwin,$(UNAME)), $(INSTALL_NAME_TOOL) -add_rpath "@executable_path/$(1)" "$(2)", $(PATCHELF) --force-rpath --set-rpath "\$$ORIGIN/$(1)" "$(2)")
endef

hls-ghc:
	$(MKDIR_P) out/
	@if test -z "$(GHC_VERSION)" ; then echo >&2 "GHC_VERSION is not set" ; false ; fi
	@if test -z "$(CABAL_PROJECT)" ; then echo >&2 "CABAL_PROJECT is not set" ; false ; fi
	$(CABAL_INSTALL) --project-file="$(CABAL_PROJECT)" -w "ghc-$(GHC_VERSION)" $(CABAL_INSTALL_ARGS) --installdir="$(ROOT_DIR)/out/$(GHC_VERSION)" exe:haskell-language-server exe:haskell-language-server-wrapper
	$(STRIP_S) "$(ROOT_DIR)/out/$(GHC_VERSION)/haskell-language-server"
	$(STRIP_S) "$(ROOT_DIR)/out/$(GHC_VERSION)/haskell-language-server-wrapper"

bindist:
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
	$(INSTALL_X) "out/$(GHC_VERSION)/haskell-language-server" "$(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION)"
	$(call set_rpath,../lib/$(GHC_VERSION),$(BINDIST_OUT_DIR)/bin/haskell-language-server-$(GHC_VERSION))
	$(SED) \
		-e "s/@@EXE_NAME@@/haskell-language-server-$(GHC_VERSION)/" \
		-e "s/@@GHC_VERSION@@/$(GHC_VERSION)/" \
		-e "s/@@BOOT_PKGS@@/$(shell ghc-pkg-$(GHC_VERSION) --global list --simple-output)/" \
		-e "s/@@ABI_HASHES@@/$(shell for dep in `ghc-pkg-$(GHC_VERSION) --global list --simple-output` ; do printf "%s:" "$$dep" && ghc-pkg-$(GHC_VERSION) field $$dep abi --simple-output ; done | tr '\n' ' ' | xargs)/" \
		bindist/wrapper.in > "$(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION).in"
	$(CHMOD_X) "$(BINDIST_OUT_DIR)/haskell-language-server-$(GHC_VERSION).in"
	$(INSTALL_D) "$(BINDIST_OUT_DIR)/bin/"
	$(INSTALL_X) "out/$(GHC_VERSION)/haskell-language-server-wrapper" "$(BINDIST_OUT_DIR)/bin/haskell-language-server-wrapper"
	$(INSTALL_D) "$(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)"
	$(FIND) "$(STORE_DIR)/ghc-$(GHC_VERSION)" -type f -name "$(DLL)" -execdir $(INSTALL_X) "{}" "$(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)/{}" \;
	$(FIND) "$(ROOT_DIR)/$(BINDIST_OUT_DIR)/lib/$(GHC_VERSION)" -type f -name '$(DLL)' -execdir $(call set_rpath,,{}) \;

version:
	@echo "$(HLS_VERSION)"

clean:
	$(RM_RF) out/*

clean-all:
	$(RM_RF) out/* $(STORE_DIR)

.PHONY: hls-ghc bindist bindist-ghc bindist-tar clean clean-all install-ghcs version
