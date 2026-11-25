# Functional core and imperative shell

Production modules live under two feature-oriented namespaces: `Baerbel.Core.*` in `src/Baerbel/Core/`, and `Baerbel.Runtime.*` in `src/Baerbel/Runtime/`. Runtime imports Core. Core imports only other Core modules and reviewed external APIs. There is no shared monad or universal command interpreter hiding effects.

The core receives immutable values: decoded events, state snapshots, settings, timestamps and provider results. It returns new values, failures or decisions. The shell reads external data, owns mutable state and resources, invokes the core, and performs the selected operations. Runtime may re-export a feature's pure symbols for callers; no compatibility modules remain at the old paths.

| Feature | Core responsibility | Runtime responsibility |
|---|---|---|
| Configuration and startup | Settings, rules, validation and service descriptions. | Read shared application/reference and private user files, open resources and construct enabled capabilities. |
| Live speech | Protocol envelopes, tool batches, transcript reduction and turn decisions. | Live WebSocket streams, queues, native audio processing and paced playback. |
| Telephony and public screening | AudioSocket/AMI values, canonical number validation, routing, ownership decisions, call transitions and restricted session configuration. | Listeners, native channel checks, dialing, media pause/reattachment and restricted session execution. |
| Personal integrations | Typed requests, provider result parsing, pagination, reference selection and profile transitions. | Authenticated requests, call-local caches and durable side effects. |
| Notes, messages and call completion | Transcript selection, delivery/result interpretation, note/PIN/end-call decisions and event cursors. | Archive reads, encoding, delivery workers, PIN input, farewell playback and hangup. |
| Persistence, authorization and shared support | Failure values, JSON codecs, request/receipt types, timestamps and metrics calculations. | Private files, SQLite transactions, token refresh, consent, logging, deadlines and cleanup. |

These are feature modules, not one large core module. For example, `Core.Live` owns `ToolState` and `stepTools`; `Runtime.Live` owns `Media`, connections and queues. `Core.Profile` owns `ProfileState` and `transition`; `Runtime.Profile` owns the `ProfileStore` capability and invokes it. `Core.HTTP` defines request/response values, while `Runtime.HTTP` sends requests. `Core.AMI` defines messages and channels, allowing pure call-state logic to avoid importing the AMI connection implementation.

`Core.PhoneNumber` validates numbers using typed libphonenumber 9.0.42 reference metadata in the `metadata` field of `config/phone-numbers.json`. The runtime loads this shared reference file beside the supplied global configuration, keeping the server's `GLOBAL USER` arguments unchanged. Application startup compiles its regex rules once and passes the resulting pure `NumberValidator` to owner dialing and home routing; regex matching automata are built lazily on first use. Calling-code selection, shared-country regions, restricted number types and rejection of canonical-prefix repairs remain data-driven. No phone-number FFI or native resource is involved. The deterministic refresh utility records upstream provenance; compatibility evidence and the retained ICU/VAD build are described in [native/README.md](../native/README.md).

## Configuration and scope

`config/global.yaml` holds application defaults, prompts, tool schemas and policy. `config/phone-numbers.json` holds reference numbering data shared by all calls. One private user file supplies the current user's credentials, integration settings and state paths. Unused imported rule snapshots and voice lists have been removed. Production releases contain the two shared configuration files and exclude development-only assets.

Production speech uses Live only. The unused STT/chat/TTS pipeline is removed; synthetic TTS belongs to the development checker, whose separate `test/live/config.json` is passed as its `DEVELOPMENT` argument. Native resampling, voice activity detection and note encoding remain runtime capabilities.

The deployed configuration selects backend `gpt-6.2-sol`, reasoning effort `low` and service tier `priority` (Fast) in `session.delegation.responses`; the Live speech model remains unchanged. Restricted sessions use the same backend settings with their existing restricted tools. Announcements retain client delegation. A focused target Live check verified two completed backend responses and an exact synthetic tool round trip; the provider reported `fast` for the requested `priority` tier. This is functionality evidence, not a latency benchmark.

Account routing resolves a registered phone to one user and its Standard or Pro type. The production configuration puts shared policy and credentials in `/etc/phone-assistant/global.yaml`. One private SQLite database holds users, preferences and PIN hashes, normalized OAuth clients and account grants, individual memory entries, delivery receipts and event cursors. User-scoped keys and foreign keys separate these resources. Google tools are instantiated only for services the selected user has connected; each connection owns its in-memory refresh cache.

The shared telephone number serves Standard accounts after matching the registered caller number. Asterisk asks Bärbel to look up the phone through local FastAGI and marks the accepted channel with its account ID. The existing per-call PIN gate still applies to protected Gmail tools. A dedicated number routes its Pro owner through the same check and other calls into restricted screening. Asterisk authenticates the shared number and Bristol on separate SIP endpoints. Bristol is selected by the called To number and restricted to A&A source addresses. Asterisk supplies an account ID for screening, handoff announcements and the optional trusted handset. The runtime verifies the account against the matched AMI channel before picking the account's services, storage or managed audio registry. Telematica remains an Asterisk carrier binding. The running dialplan supports one dedicated number and the existing optional handset. See [configuration reference](configuration.md) for file locations and ownership.

The operator provisions database accounts privately and can issue a random four-digit PIN using `phone-assistant-server --issue-pin CONFIG ACCOUNT`. This saves a salted hash and sends the PIN to the registered UK mobile through the existing SMS provider. The service reads account settings and connections at startup. The database can import an existing Google authorization export using `--connect-google`; each import targets an account and one service.

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

Continuous Live input and playback use an absolute frame deadline. `Core.Concurrent.frameSchedule` computes the next deadline from supplied timestamps: ordinary scheduler lateness does not shift the stream clock, while a stall longer than one frame rebases it to avoid a burst of overdue frames. `Runtime.Concurrent.paceStream` owns the clock read and interruptible wait. Live resets input epochs after that wait and checks output generation/pause state both before and after it; obsolete audio stays suppressed. The first delivered output establishes its subsequent deadline from actual delivery time. Worker cancellation and resource ownership are unchanged.

The shared `phone.tool` log projection includes the tool failure code alongside existing status, identifier and elapsed-time fields. It does not add tool arguments, returned content or credentials. These pacing and diagnostic corrections passed local and focused target-provider checks and remain included in the active release started at 2026-11-25T19:48:47Z.

## Persistence

The production SQLite database holds each account's Google connections,
memory entries, Gmail and Calendar receipts, SMS receipts and PBX event
cursor. Every row for these resources is scoped by account ID. A
reservation is written before a side effect. A duplicate returns the
existing receipt without repeating the side effect. Calendar checks the
receipt before scanning conflicts.

Profile reads see a coherent transaction and updates persist atomically.
Context and Windguru consume typed profile state directly. Saved values
are reference facts. The Python-era JSON profile, token exports and separate
databases are archived for recovery. Note audio comes from the existing
Asterisk mixed-call WAV. Notes select an excerpt from that recording.
The VAD remainder and bounded transcript history are retained in memory.

Windguru forecast parsing uses timestamps to address readings. A forecast array must cover every timestamp; trailing readings with no timestamp are ignored, while short arrays remain invalid. Saved-set results retain each member's independent provider failure, so one failed member does not hide available forecasts. The target checks and corrective release activation are recorded in [live-checks.md](live-checks.md#production-incidents-under-investigation).

The `asteriskconfig` installer joins SIP accounts in SQLite with the
private PBX YAML to render the files in `/etc/asterisk/phone-assistant/`.
It preserves the separate Asterisk module includes and the installed SIP
authentication and provider rules during initial adoption.

## Deployment boundary

Haskell is active in the existing `phone-assistant.service`, with Python preserved for rollback. Post-activation checks verified the installed executable, a stable process with zero restarts, owner/public/FastAGI loopback listeners and active Asterisk. The deployment does not modify the Asterisk dialplan. Independent loaded-dialplan inspection was denied by the available SSH/AMI permissions. A subsequent real owner Gmail call was attributed entirely to the deployed Haskell process; two searches succeeded and provider finalization was acknowledged. A known incoming caller also exercised forwarding/ringing, ended before answer, and generated an accepted outcome SMS. Answered bridging, unknown-caller screening, home-phone behavior and in-call voice notes remain unverified in real calls. [live-checks.md](live-checks.md) distinguishes these calls from local provider probes and synthetic sessions. See [migration.md](migration.md) for the exact deployment evidence.

Release `559d2cdf111aa2fddc3d8d1ceb6c29b0918838567a25f2a1ebf20b236ab371c1` from source `95bae18204b5da142c0f00e4f6166790d9b65a40` is active as of 2026-11-25T19:48:47Z. Verification found PID 221942 running that exact staged executable with zero restarts; it owned loopback listeners 9092, 9093 and 4573, and Asterisk was active. The private configuration remains mode 0600, contains the effective backend settings and has no obsolete TTS/STT/voice-mode keys. The separate phone reference file is installed; its contents and number policy are unchanged. The archive excludes the development checker and its configuration. Earlier real-call evidence belongs to earlier releases; this release has passed a focused target-provider check but no real Asterisk call or latency benchmark.

GHCup supplies GHC and Stack; Stack drives the build. Cabal is Stack's internal build backend. The small Setup hook resolves native library paths at configure time and introduces no runtime dependency on the build checkout.

The release contains the production commands, shared global configuration, `phone-numbers.json` and native license notices; the combined private global configuration and user database are provisioned on the miniserver. Asterisk remains an external service. Phone validation is pure Haskell with separate shared reference data; libphonenumber, protobuf and Abseil are no longer native dependencies. WebRTC VAD and ICU codecs remain linked into the executable. The standalone `scripts/build-icu.sh` preserves the existing malformed-Unicode and legacy-charset behavior. The system still supplies the standard C/C++ libraries, zlib, GMP and libsoxr; owner-note MP3 encoding requires ffmpeg. Verify the final executable with `ldd` on the target. Matching architecture alone is insufficient to prove compatibility.

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

The fixed-host deployment pipeline installs a checksum release under `/home/stefan/apps/baerbel`, prepares private configuration in the existing SSH user’s cache, and runs target library and offline executable checks without sudo. A separate privileged step checks that Asterisk has no active calls, installs the combined private global configuration in `/etc/phone-assistant` and activates the release through a removable override on the existing service. The Python unit, environment, virtual environment and shared state remain available for rollback. Preparation and activation leave Asterisk configuration untouched. The first database activation stops Bärbel before refreshing the imported memory, delivery receipts and cursors. See [the deployment pipeline](deployment.md).
