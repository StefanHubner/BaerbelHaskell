# Source inventory

Physical source lines include comments and blank lines, exclude tests, fixtures, configuration, documentation, generated Cabal metadata and third-party source/build trees.

| Scope | Files | Lines |
|---|---:|---:|
| Preserved Python runtime (`src/phone_assistant`) | 50 | 10,809 |
| Haskell runtime (`src` and `app`) | 69 | 13,561 |
| Haskell build hook, native bridge, Makefile and shell helpers | 10 | 296 |
| Current production source total | 79 | 13,857 |

The functional cleanup from `44f41b2` changes 51 production files: 1,157 added lines, 285 removed lines and 1,114 replaced lines. These counts pair old/new lines within replacement blocks and count only the excess as additions/removals; tests and documentation are excluded.

The original Python runtime has zero changed lines. Its repository guideline update added four lines. The port is larger by physical line count; shared transport, codecs, typed preparation and pure state functions are described in [the module overview](architecture.md).
