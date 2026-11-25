# Source inventory

Physical source lines include comments and blank lines, exclude tests, fixtures, configuration, documentation, generated Cabal metadata and third-party source/build trees.

| Scope | Files | Lines |
|---|---:|---:|
| Preserved Python runtime (`src/phone_assistant`) | 50 | 10,809 |
| Haskell runtime (`src` and `app`) | 70 | 13,611 |
| Haskell build hook, native bridge, Makefile and shell helpers | 10 | 296 |
| Current production source total | 80 | 13,907 |

The current purity audit from `abf5ec9` changes nine production files: 78 added lines, 29 removed lines and 81 replaced lines. These counts pair old/new lines within replacement blocks and count only the excess as additions/removals; tests and documentation are excluded. The preceding plumbing consolidation at `abf5ec9` changed 46 production files: 159 added, 170 removed and 412 replaced lines.

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

## Follow-up effect audit

The audit inspected all production modules for pure work behind effectful interfaces. It did not introduce new effect aliases or generalize signatures to conceal `IO`.

| Signature measure | Before (`abf5ec9`) | After audit |
|---|---:|---:|
| Top-level effect-returning declarations, including existing capability aliases and `ExceptT … IO` | 502 | 496 |
| Lines matching `::.*\bIO\b` in `src` and `app` | 654 | 651 |

`markDialing`, `observeStarted` and `renderPiece` now have pure signatures. Effectful dial-variable construction, profile-save preparation and mail decoder selection were replaced by pure functions, and the extra announcement-wait wrapper was removed. Gmail's local validation/forwarding adapter was removed too. New explicit local signatures describe the actual effects retained by their coordinators, which is why the textual count falls by less than the top-level count.

Pure interfaces now cover profile write/no-write decisions and byte limits, SMS request/receipt construction, Maps reference resolution, Gmail search preparation/results, shared announcement readiness and mail decoder selection. Valid UTF-8 is decoded purely. Malformed UTF-8 still uses ICU because its replacement grouping differs from the standard lenient decoder; native converters remain freshly allocated at the effect boundary.

The remaining effectful functions read or write sockets, HTTP responses, files, databases, clocks or synchronized state; allocate native codecs and call-local caches; or sequence and scope those operations. Scope constructors that allocate mutable state and resource/cancellation coordinators remain effectful. Pure capability assemblers can mention `IO` in their argument or record-field types without executing it, so a textual occurrence count also includes those interfaces.
