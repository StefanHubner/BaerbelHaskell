# Functional core and imperative shell

Production modules live under two feature-oriented namespaces: `Baerbel.Core.*` in `src/Baerbel/Core/`, and `Baerbel.Runtime.*` in `src/Baerbel/Runtime/`. Runtime imports Core. Core imports only other Core modules and reviewed external APIs. There is no shared monad or universal command interpreter hiding effects.

The core receives immutable values: decoded events, state snapshots, settings, timestamps and provider results. It returns new values, failures or decisions. The shell reads external data, owns mutable state and resources, invokes the core, and performs the selected operations. Runtime may re-export a feature's pure symbols for callers; no compatibility modules remain at the old paths.

| Feature | Core responsibility | Runtime responsibility |
|---|---|---|
| Configuration and startup | Settings, rules, validation and service descriptions. | Read the global and private user files, open resources and construct enabled capabilities. |
| Live speech and legacy pipeline | Protocol envelopes, tool batches, transcript/history reduction and turn decisions. | WebSocket/HTTP streams, queues, native audio processing and paced playback. |
| Telephony and public screening | AudioSocket/AMI values, canonical number validation, routing, ownership decisions, call transitions and restricted session configuration. | Listeners, native channel checks, dialing, media pause/reattachment and restricted session execution. |
| Personal integrations | Typed requests, provider result parsing, pagination, reference selection and profile transitions. | Authenticated requests, call-local caches and durable side effects. |
| Notes, messages and call completion | Transcript selection, delivery/result interpretation, note/PIN/end-call decisions and event cursors. | Archive reads, encoding, delivery workers, PIN input, farewell playback and hangup. |
| Persistence, authorization and shared support | Failure values, JSON codecs, request/receipt types, timestamps and metrics calculations. | Private files, SQLite transactions, token refresh, consent, logging, deadlines and cleanup. |

These are feature modules, not one large core module. For example, `Core.Live` owns `ToolState` and `stepTools`; `Runtime.Live` owns `Media`, connections and queues. `Core.Profile` owns `ProfileState` and `transition`; `Runtime.Profile` owns the `ProfileStore` capability and invokes it. `Core.HTTP` defines request/response values, while `Runtime.HTTP` sends requests. `Core.AMI` defines messages and channels, allowing pure call-state logic to avoid importing the AMI connection implementation.

`Core.PhoneNumber` validates numbers using typed libphonenumber 9.0.42 reference metadata in `rules.phone_numbers.metadata` of the shared global file. Application startup compiles its regex rules once and passes the resulting pure `NumberValidator` to owner dialing and home routing; regex matching automata are built lazily on first use. Calling-code selection, shared-country regions, restricted number types and rejection of canonical-prefix repairs remain data-driven. No phone-number FFI or native resource is involved. The deterministic refresh utility records upstream provenance; compatibility evidence and the retained ICU/VAD build are described in [native/README.md](../native/README.md).

## Values, capabilities and mutable state

An `IO` field is appropriate in a runtime capability record. The `ProfileStore` field `profileRead :: IO (Result ProfileState)` and the `Media` field `sendAudio :: ByteString -> IO (Result AudioDelivery)` explicitly describe external operations. Constructing those records is pure; executing their fields performs effects. The core receives the resulting profile or audio-delivery values, rather than the capability record.

The distinction also applies to records without an `IO` field. `Runtime.Notes.Cell` contains an `Async` handle, and runtime message jobs contain `TMVar`/`TVar` handles. Those are runtime resource/state holders. `NoteState`, `Transcript`, `ScreeningContext` and `ToolState` are immutable core values. Pure helper functions that assemble capabilities or update resource wrappers remain alongside their runtime owners.

The shell supplies clocks, random identifiers and external results explicitly. Core transitions are applied inside the existing atomic boundaries: a pure transition does not justify moving its state read or write outside an STM transaction or MVar lock. Accepted-note registration remains masked, delivery workers still drain, and unrelated transcript events still request no state write.

## Enforcing the boundary

`test/ArchitectureSpec.hs` checks that production modules are placed in Core or Runtime. For Core it rejects Runtime imports, external imports absent from [the reviewed import list](../architecture/core-imports.txt), effect types and capabilities, mutable/native handles, unsafe effect execution and effect-generating extensions. It also rejects common clock/file/output operations even when their module supplies permitted pure functions. Comments and quoted text are excluded from these source checks.

The import list is an explicit review boundary, not a claim that every export of an allowed package is pure. The source checks complement Haskell's types and code review. `package.yaml` enables `-Werror=missing-signatures`, so a missing top-level signature fails compilation. `make test` runs the architecture checks with the behavioral tests; `make check` also runs HLint, language-server diagnostics and the local configuration check. These checks do not establish production or real-call behavior.

## Permissions and lifetimes

Owner and public listeners use separate managed-media registries. An incoming socket cannot reuse an owner's paused entry. AMI binding checks native channel ownership before exposing native dialing or recording access. Public sessions receive only their restricted schemas and prompts. The runtime may perform a fixed Contacts directory lookup for public screening; this does not expose owner tools to the caller. A model, transcript, contact, email or provider response cannot choose a recipient for SMS or voice notes, a filesystem path, or a dialplan destination.

Owner ingress retains the original loopback/Asterisk trust boundary. It is not new independent caller authentication. The existing keypad PIN applies to the configured protected email tools. Successful local keypad verification updates Live before releasing the waiting protected tool, so an earlier retry prompt does not hide its returned result. Keep this limitation explicit when describing deployments.

Each call owns provider reference caches and conversation state. Account credentials, refresh caches and one prepared profile store are shared within the service scope. Pure shared service descriptions are distinct from factories that allocate call-local resources. Accepted profile operations drain their scoped worker. Accepted note/message sends have independent bounded lifetimes and drain before their underlying sender and ledger close. Async cancellation is never translated into a retry. Unknown provider outcomes remain durable and non-retryable.

Live cleanup stops and joins the bridge workers before provider finalization. If `session.start` was sent, an established session sends `session.close` and waits for `session.closed` within `rules.runtime.close_micros`; a session still starting only attempts the send within `startup_close_micros`. An already acknowledged closure needs no second request. The final `phone.session_closed` log follows this bounded exchange and records the actual acknowledgement and final usage when supplied. A timeout or transport failure does not fabricate acknowledgement, retry a call or replace the original session outcome.

## Persistence

SQLite is used for receipts and positions, not as a conversational database: Gmail sends, Calendar creates, SMS notifications and the PBX event-feed cursor. The schemas and request fingerprints preserve compatibility with the Python version. A reservation is written before a side effect. A duplicate can return the existing receipt without repeating the side effect. Calendar checks an existing receipt before scanning conflicts.

The saved profile remains a private JSON document with atomic replacement. Typed read and update capabilities share one prepared store; reads obtain a coherent descriptor snapshot without creating a mutation lock or reading the clock. Context and Windguru consume typed profile state directly. Write preparation is pure: it returns either no content for an unchanged state or bounded serialized bytes for the existing atomic writer. Its existing directory/file ownership, mode and link restrictions are preserved. Saved values are reference facts, never permissions. Note audio comes from the existing Asterisk mixed-call WAV. Notes select an excerpt rather than starting a second recorder; only the VAD remainder and bounded transcript history are retained in memory.

## Deployment boundary

This repository remains a Haskell candidate. Deployment and real-call validation are explicitly deferred; the existing Python service and Asterisk configuration remain deployed. Local validation includes real-provider probes and synthetic audio/tool sessions, recorded separately in [live-checks.md](live-checks.md). These exercise workstation/provider behavior, not SIP calls or production activation. The Core/Runtime separation and pure phone validator do not alter activation or permissions.

GHCup supplies GHC and Stack; Stack drives the build. Cabal is Stack's internal build backend. The small Setup hook resolves native library paths at configure time and introduces no runtime dependency on the build checkout.

The release contains the executable, global configuration and native license notices; the private user file and existing state are provisioned separately. Asterisk remains an external service. Phone validation is pure Haskell with reference data in the global file; libphonenumber, protobuf and Abseil are no longer native dependencies. WebRTC VAD and ICU codecs remain linked into the executable. The standalone `scripts/build-icu.sh` preserves the existing malformed-Unicode and legacy-charset behavior. The system still supplies the standard C/C++ libraries, zlib, GMP and libsoxr; owner-note MP3 encoding requires ffmpeg. Verify the final executable with `ldd` on the target. Matching architecture alone is insufficient to prove compatibility.

The shared HTTP manager disables automatic request replay and environment proxies. Configured mutation outcomes distinguish authentication failure, definite rejection and an unknown accepted-send outcome. Typed request bodies and derived JSON codecs keep serialization at shared boundaries.

## Shared functionality

- `Core.HTTP` holds typed bodies, requests and responses. `Core.Provider.get` and `post` build requests; Core provider modules parse results. `Runtime.HTTP` owns TLS managers, authentication headers, response bounds and mutation uncertainty. Its `requestJSON` runner accepts a pure response parser. `Runtime.Provider.boundedMap` runs bounded concurrent effects.
- `Runtime.Effects` centralizes failure dispatch, pure result mapping, short-circuit action folds, operational exception boundaries, resource cleanup and deadline failures. Its execution functions retain explicit `IO` types. `Core.Concurrent` supplies pure elapsed-time/frame-delay calculations; `Runtime.Concurrent` owns waits and commits pure MVar transitions.
- `Core.JSON` centralizes typed codecs, strict argument parsing and optional event-field extraction. `Core.Protocol` builds protocol envelopes. Runtime transport modules serialize and send those values.
- Core cache rules and projections are distinct from `Runtime.Cache` memoization. Credentials are cached at account scope; provider references and owner addresses are cached at call scope. Contacts warm-up retains serialized initialization and retry-after-failure behavior.
- Core profile, ledger and SMS functions compute values and reservation decisions. `Runtime.Storage` owns private-file and atomic-write mechanics; runtime ledger/SMS modules retain their existing SQLite transaction boundaries and distinct receipt schemas.
- Core MIME, mail-content and encoder preparation handle immutable message data. Runtime charset conversion and encoding own native resources and the ffmpeg process. Valid UTF-8 and literal header text need no converter; stateful legacy codecs and malformed Unicode replacement retain their boundary behavior.

Tool payloads are decoded at their input boundary into typed commands and prepared requests. Calendar dates, Maps requests, Windguru windows and recording intervals are prepared once and reused by the effectful steps. Help is a pure value computed when the call's available tools are assembled; it has no internal tool-name or unused-argument checks. Internal direct-call modes, hangup reasons and end-call reasons are algebraic data types.

Pure functions handle audio framing, stream preparation, transcript reduction, PIN state transitions, message admission, cursor positions and reconnect outcomes. HTTP URL preparation and response projection are pure too. OAuth refresh and consent responses are validated and projected using explicitly supplied timestamps; their coordinators retain the original request/clock order. Provider results, Calendar pagination cursors, conversation history and note rejection replies are computed from explicit values. AudioSocket identity extraction and managed-media packet decisions consume already read packets. SMS retry decisions consume database rows inside the existing transaction; profile projections consume typed snapshots.

Dial variables and dialing transitions, session-start classification, announcement readiness, Gmail search preparation/results, Maps reference lookup and SMS request/receipt construction have pure interfaces.

The surrounding I/O coordinators fetch data, read clocks/state and commit the selected changes. Transcript transformations indicate when no update is needed, so unrelated events do not write shared state or wake waiters. Generic codecs and shared result constructors handle routine JSON envelopes.

Functional state transitions return new values and decisions. Their small I/O callers commit the state or execute the selected operation. Shared deadline helpers take the complete failure value so uncertainty is preserved. Provider-specific mutation receipts, timestamped forecast caches and timeout recovery retain their own pure interpretation. Library exceptions are converted to typed failures at operational boundaries; asynchronous cancellation retains its resource-cleanup meaning.

The deployment Makefile stages immutable releases and activates one through a removable systemd override. The existing Python unit, environment, virtual environment and shared state remain available for rollback. Installation and activation are separate commands; neither modifies Asterisk configuration. See [the deployment pipeline](deployment.md).
