#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
work="$root/.native/icu-build"
mkdir -p "$work" "$root/.native/lib"
archive="$work/icu.tar.gz"
read -r _ version digest < "$root/native/build-sources.txt"
release=${version//./-}
source_version=${version//./_}
url="https://github.com/unicode-org/icu/releases/download/release-$release/icu4c-$source_version-src.tgz"
if [ ! -f "$archive" ]; then curl --fail --location "$url" --output "$archive"; fi
printf '%s  %s\n' "$digest" "$archive" | sha256sum --check --status
tar -xzf "$archive" -C "$work"
mkdir -p "$work/build"
(
  cd "$work/build"
  CC=gcc CXX=g++ CFLAGS=-fPIC CXXFLAGS=-fPIC "$work/icu/source/configure" \
    --enable-static --disable-shared --disable-tests --disable-samples --with-data-packaging=static
  make -j4
)
cp "$work"/build/lib/libicu{uc,i18n,data}.a "$root/.native/lib/"
