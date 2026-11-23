#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
./scripts/prepare-build.sh
stack build --test --no-run-tests
install_root=$(stack path --local-install-root)
stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
mkdir -p "$stage/baerbel/bin" "$stage/baerbel/config" "$stage/baerbel/native" "$stage/baerbel/deploy" "$root/release"
install -m 0755 "$install_root/bin/phone-assistant-server" "$stage/baerbel/bin/"
install -m 0755 "$install_root/bin/phone-assistant-gmail-auth" "$stage/baerbel/bin/"
strip "$stage/baerbel/bin/phone-assistant-server" "$stage/baerbel/bin/phone-assistant-gmail-auth"
install -m 0644 config/global.json config/user.example.json "$stage/baerbel/config/"
cp -r native/licenses "$stage/baerbel/native/"
install -m 0644 deploy/phone-assistant.service deploy/phone-assistant.logrotate "$stage/baerbel/deploy/"
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
