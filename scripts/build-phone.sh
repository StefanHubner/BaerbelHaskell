#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
work="$root/.native/phone-build"
mkdir -p "$work" "$root/.native/lib"
read -r version digest < "$root/native/phone-source.txt"
fetch() {
  local url=$1 archive=$2 digest=$3
  if [ ! -f "$archive" ]; then curl --fail --location "$url" --output "$archive"; fi
  printf '%s  %s\n' "$digest" "$archive" | sha256sum --check --status
  tar -xzf "$archive" -C "$work"
}
fetch "https://github.com/google/libphonenumber/archive/refs/tags/v$version.tar.gz" \
  "$work/libphonenumber.tar.gz" "$digest"
fetch "https://codeload.github.com/abseil/abseil-cpp/tar.gz/refs/tags/20210324.2" \
  "$work/abseil.tar.gz" "59b862f50e710277f8ede96f083a5bb8d7c9595376146838b9580be90374ee1f"
fetch "https://codeload.github.com/protocolbuffers/protobuf/tar.gz/refs/tags/v3.12.4" \
  "$work/protobuf.tar.gz" "512e5a674bf31f8b7928a64d8adf73ee67b8fe88339ad29adaa3b84dbaa570d8"
fetch "https://github.com/unicode-org/icu/releases/download/release-70-1/icu4c-70_1-src.tgz" \
  "$work/icu.tar.gz" "8d205428c17bf13bb535300669ed28b338a157b1c01ae66d31d0d3e2d47c3fd5"
(
  cd "$work"
  apt-get download libprotobuf-dev protobuf-compiler libprotobuf23 libprotoc23 libabsl-dev libabsl20210324
  for package in ./*.deb; do dpkg-deb -x "$package" deps; done
)
deps="$work/deps/usr"
libdir="$deps/lib/x86_64-linux-gnu"
source="$work/libphonenumber-$version/cpp"
export LD_LIBRARY_PATH="$libdir${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
cp "$deps/include/absl/base/options.h" "$work/abseil-cpp-20210324.2/absl/base/options.h"
cmake -S "$work/abseil-cpp-20210324.2" -B "$work/absl-build" \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON -DABSL_BUILD_TESTING=OFF -DBUILD_SHARED_LIBS=OFF
cmake --build "$work/absl-build" --parallel 4
cmake -S "$work/protobuf-3.12.4/cmake" -B "$work/protobuf-build" \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON -Dprotobuf_BUILD_TESTS=OFF -Dprotobuf_BUILD_SHARED_LIBS=OFF
cmake --build "$work/protobuf-build" --target libprotobuf --parallel 4
mkdir -p "$work/icu-build"
(
  cd "$work/icu-build"
  CC=gcc CXX=g++ CFLAGS=-fPIC CXXFLAGS=-fPIC "$work/icu/source/configure" \
    --enable-static --disable-shared --disable-tests --disable-samples --with-data-packaging=static
  make -j4
)
cmake -S "$source" -B "$work/build" -DCMAKE_PREFIX_PATH="$deps" \
  -DPROTOC_BIN="$deps/bin/protoc" -DPROTOBUF_LIB="$work/protobuf-build/libprotobuf.a" \
  -DUSE_BOOST=OFF -DUSE_STDMUTEX=ON -DBUILD_TESTING=OFF \
  -DBUILD_GEOCODER=OFF -DREGENERATE_METADATA=OFF -DBUILD_SHARED_LIBS=OFF \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON
cmake --build "$work/build" --parallel 4
g++ -std=c++11 -fPIC -c -I"$source/src" -I"$deps/include" \
  "$root/cbits/phone_number.cpp" -o "$work/phone_number.o"
{
  printf 'create %s\naddmod %s\n' "$root/.native/lib/libbaerbel-phone.a" "$work/phone_number.o"
  printf 'addlib %s\n' "$work/build/libphonenumber.a" "$work/protobuf-build/libprotobuf.a"
  find "$work/absl-build" -name 'libabsl*.a' -printf 'addlib %p\n'
  for library in "$work"/icu-build/lib/libicu{i18n,uc,data}.a; do printf 'addlib %s\n' "$library"; done
  printf 'save\nend\n'
} > "$work/merge.mri"
ar -M < "$work/merge.mri"
g++ -shared "$work/phone_number.o" "$root/.native/lib/libbaerbel-phone.a" -lz -o "$work/link-check.so"

# Also resolve the standard text codec dependency from the bundled PIC ICU.
cp "$work"/icu-build/lib/libicu{uc,i18n,data}.a "$root/.native/lib/"
