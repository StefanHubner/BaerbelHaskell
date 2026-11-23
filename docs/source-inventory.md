# Source inventory

Physical source lines include comments and blank lines, exclude tests, fixtures, configuration, documentation, generated Cabal metadata and third-party source/build trees.

| Scope | Files | Lines |
|---|---:|---:|
| Preserved Python runtime (`src/phone_assistant`) | 50 | 10,809 |
| Haskell runtime (`src` and `app`) | 69 | 12,689 |
| Haskell build hook, native bridge, Makefile and shell helpers | 10 | 296 |
| New production source total | 79 | 12,985 |

The new repository adds these 12,985 production source lines and removes zero existing source lines. The original Python runtime has zero changed lines. Its repository guideline update added four lines. The port is larger by this measure; its shared transport, codec, persistence and pure state functions are described in [the module overview](architecture.md). Further purity cleanup identified during review remains unfinished.
