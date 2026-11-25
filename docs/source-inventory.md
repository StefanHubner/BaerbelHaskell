# Source inventory

Physical source lines include comments and blank lines, exclude tests, fixtures, configuration, documentation, generated Cabal metadata and third-party source/build trees.

| Scope | Files | Lines |
|---|---:|---:|
| Preserved Python runtime (`src/phone_assistant`) | 50 | 10,809 |
| Haskell runtime (`src` and `app`) | 70 | 13,562 |
| Haskell build hook, native bridge, Makefile and shell helpers | 10 | 296 |
| Current production source total | 80 | 13,858 |

The plumbing consolidation from `028cf7e` changes 46 production files: 159 added lines, 170 removed lines and 412 replaced lines. These counts pair old/new lines within replacement blocks and count only the excess as additions/removals; tests and documentation are excluded.

The original Python runtime has zero changed lines. Its repository guideline update added four lines. The port is larger by physical line count; shared transport, codecs, typed preparation and pure state functions are described in [the module overview](architecture.md).

## Pure-result refactor at `028cf7e`

That refactor converted 44 existing functions to pure result types. Thirteen effect wrappers were removed; one shared context-update function centralizes the remaining writes. Existing coordinators retain the actual HTTP, socket, clock, storage and state operations.

| Signature measure | Before (`165b069`) | After (`028cf7e`) |
|---|---:|---:|
| Top-level effect-returning declarations, including capability aliases and `ExceptT … IO` | 553 | 497 |
| Lines matching `::.*\bIO\b` in `src` and `app` | 709 | 647 |

The first count excludes foreign imports, record fields and local helpers. The second includes local signatures, capability fields and functions that only accept an effect as an argument; it is a reproducible text count, not a count of runtime operations. Neither measure hides effects behind new type aliases or polymorphic monad constraints.

The main conversions cover shared request builders/decoders, provider normalization and pagination, token responses, audio packet decisions, transcript/history transformations, note replies, SMS reservation decisions and profile projections. Native codec allocation, synchronization, clocks and network/file operations remain effectful.

## Shared plumbing

The subsequent consolidation reuses the same small functions throughout startup, providers, telephony, streaming, storage and native boundaries:

- `HTTP.requestJSON`: execute a JSON request and apply a pure payload parser; transport errors pass through unchanged.
- `Effects.continue`, `mapResult`, `foldResults` and `runAll`: failure dispatch and short-circuit execution without copying error branches into each operation.
- `Effects.attempt`, `within` and `withResult`: operational exception handling, timeout outcomes and scoped resource cleanup. Cancellation remains asynchronous; the complete timeout failure retains its uncertainty flag.
- `Concurrent.stateMVar` and `updateMVar`: commit pure state transformations under the original locking semantics.
- `Effects.monotonicSeconds` and `Concurrent.paceFrame`: shared clock conversion and frame pacing; elapsed-time arithmetic is pure.
- `JSON.field`, `textField`, `numberField` and `eventDelta`: shared pure extraction from external events.
- `Cache.onceSuccess`: shared initialization caching, now also used for Contacts warm-up.

Provider-specific response interpretation, receipt uncertainty and timeout recovery remain explicit. The obsolete `Provider.fetch` adapter and duplicate warm-up, clock, getter and sequencing implementations were removed. Pure functions still receive ordinary values; effect runners retain explicit `IO` types.
