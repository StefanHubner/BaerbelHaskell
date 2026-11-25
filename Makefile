SHELL := /bin/bash
.DEFAULT_GOAL := test

TARGET ?=
INSTALL_PREFIX ?= /opt/baerbel-hs
USER_CONFIG ?=
RELEASE_ARCHIVE ?= release/baerbel-linux-x86_64.tar.gz

.PHONY: prepare native build test check package install activate rollback

prepare:
	./scripts/prepare-build.sh

native: prepare
	test -f .native/lib/libbaerbel-phone.a || ./scripts/build-phone.sh
	test -f .native/lib/libbaerbel-vad.a || ./scripts/build-vad.sh

build: native
	stack build --test --no-run-tests

test: native
	stack test

check: test
	@test -n "$(USER_CONFIG)" || { printf 'Set USER_CONFIG to a private JSON file outside the repository.\n' >&2; exit 2; }
	.tools/hlint src app test Setup.hs
	haskell-language-server-wrapper typecheck src app test Setup.hs --cwd "$(CURDIR)"
	stack exec phone-assistant-server -- --check config/global.json "$(USER_CONFIG)"

package: native
	./scripts/package-release.sh

# Install stages the already reviewed archive. Activation is a separate action.
install:
	./scripts/deploy.sh install "$(TARGET)" "$(INSTALL_PREFIX)" "$(RELEASE_ARCHIVE)"

activate:
	./scripts/deploy.sh activate "$(TARGET)" "$(INSTALL_PREFIX)"

rollback:
	./scripts/deploy.sh rollback "$(TARGET)" "$(INSTALL_PREFIX)"
