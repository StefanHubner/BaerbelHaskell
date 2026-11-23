# Native telephone number validation

The Haskell call-routing boundary delegates parsing, canonical formatting and
number classification to Google's libphonenumber. The implementation uses the
same 9.0.42 metadata as the preserved Python baseline. The source archive SHA-256
is recorded in `phone-source.txt`; this is a compatibility artifact, not an
application dependency version constraint.

Run `scripts/build-phone.sh` on the Ubuntu 22.04 x86-64 build host. It needs curl,
CMake, g++, binutils, the existing ICU development package, and apt/dpkg download
tools. It downloads and extracts protobuf/Abseil build packages into `.native/`
without root installation. Matching upstream Abseil, protobuf and ICU sources
are rebuilt with position independent code so GHC can also link its shared
library; Debian static archives use TLS relocations unsuitable for that link.
Every upstream archive is verified with the SHA-256 recorded in the build script.
The final archive includes libphonenumber, protobuf,
Abseil and ICU objects, and the ICU archives also serve the standard text codec package, so the executable has no runtime dependency on their
versioned shared libraries. The standard C/C++ runtime and zlib remain system
dependencies. Generated artifacts and downloaded sources are excluded from Git.

`test/fixtures/phone_numbers.json` contains 864 public library example numbers
and malformed/premium cases, generated from the original Python normalization
boundary. Native validation matched all of them. These fixtures cover many
countries and types; they do not prove equivalence for all possible numbers.

The bridge is deliberately limited to fixed-line, mobile, and fixed-or-mobile
numbers. A parsed number that would be silently repaired, including a domestic
trunk zero, is rejected. International provider routing remains pure Haskell.

Upstream source: https://github.com/google/libphonenumber/tree/v9.0.42
Upstream licence: Apache-2.0 (included in downloaded source archive).

Build dependencies use Abseil 20210324.2, protobuf 3.12.4 and ICU 70.1,
matching the Ubuntu 22.04 headers and ABI. Their source digests are recorded in
`build-sources.txt`; these versions are native compatibility inputs. A shared
link probe verifies PIC suitability before a build is considered successful.
License notices for the bundled native code are in `native/licenses/`.

WebRTC VAD uses the same 2.0.14 C sources as the Python baseline; `scripts/build-vad.sh` verifies the source archive before compiling. The bundled license contains the wrapper MIT notice and WebRTC BSD notice. Stateful MIME codecs allocate fresh converters inside a small I/O boundary; converters are never shared as supposedly pure values.
