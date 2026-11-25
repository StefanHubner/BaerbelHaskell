# Source inventory

Physical source lines include comments and blank lines, exclude tests, fixtures, configuration, documentation, generated Cabal metadata and third-party source/build trees.

| Scope | Files | Lines |
|---|---:|---:|
| Preserved Python runtime (`src/phone_assistant`) | 50 | 10,809 |
| Haskell runtime (`src` and `app`) | 69 | 13,573 |
| Haskell build hook, native bridge, Makefile and shell helpers | 10 | 296 |
| Current production source total | 79 | 13,869 |

The pure-result refactor from `165b069` changes 30 production files: 205 added lines, 193 removed lines and 334 replaced lines. These counts pair old/new lines within replacement blocks and count only the excess as additions/removals; tests and documentation are excluded.

The original Python runtime has zero changed lines. Its repository guideline update added four lines. The port is larger by physical line count; shared transport, codecs, typed preparation and pure state functions are described in [the module overview](architecture.md).

## Pure-result refactor

44 existing functions now have pure result types. Thirteen effect wrappers were removed; one shared context-update function centralizes the remaining writes. Existing coordinators retain the actual HTTP, socket, clock, storage and state operations.

| Signature measure | Before (`165b069`) | After |
|---|---:|---:|
| Top-level effect-returning declarations, including capability aliases and `ExceptT … IO` | 553 | 497 |
| Lines matching `::.*\bIO\b` in `src` and `app` | 709 | 647 |

The first count excludes foreign imports, record fields and local helpers. The second includes local signatures, capability fields and functions that only accept an effect as an argument; it is a reproducible text count, not a count of runtime operations. Neither measure hides effects behind new type aliases or polymorphic monad constraints.

The main conversions cover shared request builders/decoders, provider normalization and pagination, token responses, audio packet decisions, transcript/history transformations, note replies, SMS reservation decisions and profile projections. Native codec allocation, synchronization, clocks and network/file operations remain effectful.
