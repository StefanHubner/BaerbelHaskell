#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
source_status=$(git status --porcelain)
[[ -z "$source_status" ]] || { printf 'Commit or remove source changes before packaging.\n' >&2; exit 2; }
./scripts/prepare-build.sh
stack build --test --no-run-tests
install_root=$(stack path --local-install-root)
stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
mkdir -p "$stage/baerbel/bin" "$stage/baerbel/config" "$stage/baerbel/native" "$stage/baerbel/deploy" "$root/release"
install -m 0755 "$install_root/bin/phone-assistant-server" "$stage/baerbel/bin/"
install -m 0755 "$install_root/bin/phone-assistant-gmail-auth" "$stage/baerbel/bin/"
install -m 0755 "$install_root/bin/asteriskconfig" "$stage/baerbel/bin/"
strip "$stage/baerbel/bin/phone-assistant-server" "$stage/baerbel/bin/phone-assistant-gmail-auth" "$stage/baerbel/bin/asteriskconfig"
install -m 0644 config/global.yaml config/instructions.yaml config/phone-numbers.json config/users.sql "$stage/baerbel/config/"
install -m 0644 scripts/migrate-users.py deploy/file-storage.yaml "$stage/baerbel/deploy/"
install -d "$stage/baerbel/config/asterisk" "$stage/baerbel/config/tools"
install -m 0644 config/tools/*.yaml "$stage/baerbel/config/tools/"
install -m 0644 config/asterisk/*.conf.in "$stage/baerbel/config/asterisk/"
cp -r native/licenses "$stage/baerbel/native/"
install -m 0644 deploy/phone-assistant.service deploy/phone-assistant.logrotate "$stage/baerbel/deploy/"
install -D -m 0644 deploy/asterisk/account-entry.conf.in "$stage/baerbel/deploy/asterisk/account-entry.conf.in"
install -m 0644 scripts/render-asterisk-accounts.py "$stage/baerbel/deploy/asterisk/"
{
  git rev-parse HEAD
  ghc --numeric-version
  stack --numeric-version
  uname -m
  getconf GNU_LIBC_VERSION
  env -u LD_PRELOAD ldd "$stage/baerbel/bin/phone-assistant-server"
} > "$stage/baerbel/BUILD.txt"
tar -C "$stage" -czf "$root/release/baerbel-linux-x86_64.tar.gz" baerbel
sha256sum "$root/release/baerbel-linux-x86_64.tar.gz"
