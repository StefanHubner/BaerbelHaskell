#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
source_dir=.native/vad
archive=.native/webrtcvad.tar.gz
mkdir -p "$source_dir" .native/lib .native/vad-objects
if [[ ! -f "$archive" ]]; then
  curl --fail --location --silent --show-error --output "$archive" https://files.pythonhosted.org/packages/28/ba/3a8ce2cff3eee72a39ed190e5f9dac792da1526909c97a11589590b21739/webrtcvad_wheels-2.0.14.tar.gz
fi
printf '%s  %s\n' 5f59c8e291c6ef102d9f39532982fbf26a52ce2de6328382e2654b0960fea397 "$archive" | sha256sum --check --status
tar -xzf "$archive" -C "$source_dir" --strip-components=1
for source in "$source_dir"/cbits/webrtc/common_audio/{signal_processing,vad}/*.c; do
  object=.native/vad-objects/$(basename "${source%.c}").o
  cc -O2 -fPIC -DWEBRTC_POSIX -I"$source_dir/cbits" -c "$source" -o "$object"
done
ar rcs .native/lib/libbaerbel-vad.a .native/vad-objects/*.o
