# Telephone metadata and native codecs

Telephone validation is pure Haskell in `Baerbel.Core.PhoneNumber`. The shared
`config/global.json` contains the same libphonenumber 9.0.42 metadata as the
preserved Python baseline, with the upstream URL and XML SHA-256 under
`rules.phone_numbers.metadata`. Regular expressions are compiled when the
validator is constructed at application startup and shared across calls;
regex-tdfa constructs its matching automata lazily on first use.

The validator preserves country-code and shared-region selection, national
number lengths, restricted-type precedence and canonical-prefix rejection.
Only fixed-line, mobile and fixed-or-mobile numbers are permitted. Numbers
that libphonenumber would silently repair, including domestic trunk prefixes,
are rejected. The C++ bridge, libphonenumber, protobuf and Abseil are no longer
build or runtime dependencies. The libphonenumber Apache-2.0 notice is retained
for the reference metadata.

To refresh the metadata after reviewing an upstream revision, download its
`resources/PhoneNumberMetadata.xml`, then run:

```sh
python3 scripts/update-phone-metadata.py 9.0.42 /tmp/PhoneNumberMetadata.xml config/global.json
make test
```

The converter uses Python's standard library, records source provenance and
rejects unsupported regex/transform syntax instead of silently changing it.
It translates digit classes and noncapturing groups to POSIX extended regexes,
including the adjusted group indices used by national-prefix replacements.
Country-specific rules remain data, rather than Haskell branches.

`test/fixtures/phone_numbers.json` retains 864 public Python library examples
and malformed/restricted cases. Before removing the native bridge, the pure
validator also matched the existing 9.0.42 C++ implementation on 1,236,669
unique generated cases: 148,339 permitted, 174,997 restricted, 913,333 invalid.
The differential included examples across every metadata territory and number
type, digit substitutions, truncations, added prefixes and seeded samples from
number patterns. No mismatches were found. This is broad compatibility
evidence, not proof for every possible number; repeat a differential check
when changing metadata or regex interpretation.

ICU remains necessary for the existing malformed-Unicode and legacy-charset
email decoding behavior. Run `scripts/build-icu.sh` on the Ubuntu 22.04 x86-64
build host with curl, gcc, g++, make and binutils installed. It verifies the
archive against `build-sources.txt` and builds PIC static ICU archives under
`.native/`, outside Dropbox through `scripts/prepare-build.sh`. ICU 70.1 matches
the build host's headers and ABI; this version is a native compatibility input.
The `text-icu` package also needs the host's ICU development headers. No
apt/dpkg downloads, protobuf tooling or CMake are needed for this build.

WebRTC VAD retains the same 2.0.14 C sources as the Python baseline;
`scripts/build-vad.sh` verifies the archive before compiling. The bundled
license contains the wrapper MIT notice and WebRTC BSD notice. Stateful MIME
codecs allocate fresh converters inside a small I/O boundary; converters are
never shared as supposedly pure values. License notices for retained code and
metadata are in `native/licenses/`.
