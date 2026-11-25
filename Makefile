SHELL := /bin/bash
.DEFAULT_GOAL := test

user_config := $(HOME)/.config/baerbel/user.json

.PHONY: prepare native build test check live-check live-delivery-check package install activate activate-accounts rollback

prepare:
	./scripts/prepare-build.sh

native: prepare
	test -f .native/lib/libicuuc.a || ./scripts/build-icu.sh
	test -f .native/lib/libbaerbel-vad.a || ./scripts/build-vad.sh

build: native
	stack build --test --no-run-tests

test: native
	stack test

check: test
	.tools/hlint src app test Setup.hs
	haskell-language-server-wrapper typecheck src app test Setup.hs --cwd "$(CURDIR)"
	stack exec phone-assistant-server -- --check config/global.json "$(user_config)"

live-check: build
	stack exec baerbel-live-check -- config/global.json "$(user_config)" test/live/config.json

live-delivery-check: build
	stack exec baerbel-live-check -- delivery config/global.json "$(user_config)" test/live/config.json

package: native
	./scripts/package-release.sh

# Install stages the already reviewed archive. Activation is a separate action.
install:
	./scripts/deploy.sh install

activate:
	./scripts/deploy.sh activate

activate-accounts:
	./scripts/deploy.sh activate-accounts

rollback:
	./scripts/deploy.sh rollback
