CWD     := $(shell pwd)
UNAME   := $(shell uname -s)
ARCH    := $(shell uname -m)

CABAL_BUILD_FLAGS   ?= -j --enable-static
CABAL_INSTALL_FLAGS ?= $(CABAL_BUILD_FLAGS)

CABAL         := cabal
BINDIR        := ${CWD}/cabalversion2

# Used only for windows, to find the right Program Files.
PROGRAM_FILES = Program\ Files\ \(x86\)
# Windows installer tools; assumes running on Cygwin and using WiX 3.10
WiX      := /cygdrive/c/${PROGRAM_FILES}/WiX\ Toolset\ v3.10
CANDLE   := ${WiX}/bin/candle.exe
HEAT     := ${WiX}/bin/heat.exe
LIGHT    := ${WiX}/bin/light.exe

REV         ?= $(shell git rev-parse --short=7 HEAD || echo "unknown")
VERSION     := $(shell grep -i ^Version caw/caw.cabal | awk '{ print $$2}')
SYSTEM_DESC ?= ${UNAME}-${ARCH}_${REV}
PKG         := caw-${VERSION}-${SYSTEM_DESC}

ifneq (,$(findstring _NT,${UNAME}))
    DIST := ${PKG}.msi
    EXE_EXT := .exe
    # For a systemwide distribution .msi, use:
    # PREFIX ?= ${PROGRAM_FILES}/Galois/caw\ ${VERSION}
    # split this up because `cabal copy` strips drive letters
    PREFIX_ABS    := /cygdrive/c/${PREFIX}
    # since Windows installs aren't overlapping like /usr/local, we
    # don't need this extra prefix
    PREFIX_SHARE  :=
    # goes under the share prefix
    PREFIX_DOC    := /doc
    PKG_PREFIX    := ${PKG}/${PREFIX}
    ROOT_PATH     := /cygdrive/c

    # The windows build bot objects to many packages
    CABAL_FREEZE  := NOFREEZE
else
    DIST := ${PKG}.tar.gz ${PKG}.zip
    EXE_EXT :=
    # For a systemwide distribution like an .rpm or .pkg, use something like:
    # PREFIX ?= /usr/local
    PREFIX_ABS := ${PREFIX}
    PREFIX_SHARE := /share
    # goes under the share prefix
    PREFIX_DOC   := /doc/caw
    PKG_PREFIX   := ${PKG}${PREFIX}
    ROOT_PATH    := /
    CABAL_FREEZE  := cabal.project.freeze
endif

.PHONY: cabal2
cabal2:
	$(CABAL) update
	mkdir -p ${BINDIR}
	$(CABAL) install cabal-install-2.0.0.1 --bindir=${BINDIR}
	PATH="${BINDIR}:$(PATH)" cabal --version
	PATH="${BINDIR}:$(PATH)" cabal update

.PHONY: happy
happy: cabal2
	PATH="${BINDIR}:$(PATH)" cabal install happy --bindir=${BINDIR}

.PHONY: alex
alex: cabal2
	PATH="${BINDIR}:$(PATH)" cabal install alex --bindir=${BINDIR}

cabal.project.freeze:
	cp cabal.project.freeze-${VERSION} ${CABAL_FREEZE}
	touch cabal.project.freeze

.PHONY: docs
docs:

.PHONY: dist
dist: ${DIST}

CAW_EXE_DIR := ./release
CAW_EXE  := ${CAW_EXE_DIR}/caw${EXE_EXT}

CAW_SRC := \
  $(shell find caw/src \
            \( -name \*.hs -or -name \*.lhs -or -name \*.x -or -name \*.y \) \
            -and \( -not -name \*\#\* \) -print) \
  ${GIT_INFO_FILES}

.PHONY: ${CAW_EXE}
${CAW_EXE}: cabal2 happy alex
	PATH="${BINDIR}:$(PATH)" cabal sandbox init
	mkdir deps
	git clone https://github.com/elliottt/llvm-pretty deps/llvm-pretty
	git clone https://github.com/GaloisInc/llvm-pretty-bc-parser deps/llvm-pretty-bc-parser
	PATH="${BINDIR}:$(PATH)" cabal sandbox add-source caw caw-api caw-build deps/llvm-pretty deps/llvm-pretty-bc-parser
	PATH="${BINDIR}:$(PATH)" cabal install caw
	mkdir -p ${CAW_EXE_DIR}
	cp .cabal-sandbox/bin/caw${EXE_EXT} ${CAW_EXE}
# ${CAW_EXE}: cabal.project.freeze cabal2
# 	PATH="${BINDIR}:$(PATH)" cabal --version
# 	PATH="${BINDIR}:$(PATH)" ./build.sh
# 	mkdir -p ${CAW_EXE_DIR}
# 	cp $$(find dist-newstyle -type f -name caw${EXE_EXT}) ${CAW_EXE}

${PKG}: ${CAW_EXE}
	cp ${CAW_EXE} ${PKG}

${PKG}.tar.gz: ${PKG}
	tar -czvf $@ $<

${PKG}.zip: ${PKG}
	zip -r $@ $<

${PKG}.msi: ${PKG} win32/caw.wxs
	${HEAT} dir ${PKG_PREFIX} -o allfiles.wxs -nologo -var var.pkg \
          -ag -wixvar -cg ALLFILES -srd -dr INSTALLDIR -sfrag
	${CANDLE} -ext WixUIExtension -ext WixUtilExtension            \
          -dversion=${VERSION} -dpkg=${PKG_PREFIX} win32/caw.wxs
	${CANDLE} -ext WixUIExtension -ext WixUtilExtension            \
          -dversion=${VERSION} -dpkg=${PKG_PREFIX} allfiles.wxs
	${LIGHT} -ext WixUIExtension -ext WixUtilExtension             \
	  -sval -o $@ caw.wixobj allfiles.wixobj
	rm -f allfiles.wxs
	rm -f *.wixobj
	rm -f *.wixpdb

.PHONY: clean
clean:
	rm -rf caw-${VERSION}*/
	rm -rf caw-${VERSION}*.tar.gz
	rm -rf caw-${VERSION}*.zip
	rm -rf caw-${VERSION}*.msi

.PHONY: squeaky
squeaky: clean
	rm -rf dist
	rm -rf dist-newstyle
