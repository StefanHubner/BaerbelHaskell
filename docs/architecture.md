# Functional implementation

The application passes explicit capabilities to call-scoped handlers. Pure functions decode and normalize data, choose routes, reduce protocol events, select note excerpts and produce commands. Small interpreters perform HTTP, WebSocket, AMI, audio and storage operations. Capabilities are ordinary functions and immutable records; they are not inheritance hierarchies.

| Area | Modules | Responsibility |
|---|---|---|
| Startup and configuration | Application, CLI, Config, ConfigPaths, Validation, Services | Merge one global file and one private user file, validate external settings, scope resources and construct enabled capabilities. |
| Shared boundaries | Error, Effects, JSON, HTTP, WebSocket, Concurrent, Cache, Logging, AudioMetrics | Explicit error values, typed codecs, bounded transport, cancellation, scoped logs, delivered-frame metrics and reusable scheduling/cache behavior. |
| Live speech | Protocol, Live, Background, Session | Session configuration, completed tool batches, replay handling, background progress, owner call orchestration. |
| Legacy speech | Chat, Pipeline, Speech, VAD, Ringback | Pure sentence/turn logic with streamed STT, responses and paced TTS; native compatible resampling and VAD. |
| Telephony | AudioSocket, AMI, Routing, CallState, Telephony, ManagedMedia, PhoneNumber | Frame protocol, native ownership, provider choice, dialing, control-only recovery, media pause and reattachment. |
| Public calls | Incoming, IncomingSession, Directory, Messages, SMS, Events | Restricted screening, trusted-number lookup, fixed-owner messages, durable SMS deduplication and PBX-event replay. |
| Personal integrations | Gmail, Calendar, Contacts, Maps, MapsRoute, Windguru, Profile | Typed tool operations and pure provider normalization; limits and prompts are configuration data. |
| Shared provider details | Provider, ProviderConfig, GoogleValidation, MIME, MailContent, MapTypes, OpeningHours, TimeZones, TimeFormat | Common validators, schemas, MIME/HTML handling, calendar zones and walking-route data. |
| Notes and hangup | Notes, NoteRuntime, Recording, Encoder, NoteMail, EndCall, Pin | Pure note/end-call decisions with archive reads, MP3 delivery, controlled farewell and keypad authorization. |
| Persistent state and authorization | Storage, Store, Ledger, OAuth, Authorize | Private files, atomic profile replacement, receipt reservations, one credential boundary, cached refresh and loopback PKCE consent. |

## Permissions and lifetimes

Owner and public listeners use separate managed-media registries. An incoming socket cannot reuse an owner's paused entry. AMI binding checks native channel ownership before exposing native dialing or recording access. Public sessions receive only their restricted schemas and prompts. A model, transcript, contact, email or provider response cannot choose a recipient for SMS or voice notes, a filesystem path, or a dialplan destination.

Owner ingress retains the original loopback/Asterisk trust boundary. It is not new independent caller authentication. The existing keypad PIN applies to the configured protected email tools. Keep this limitation explicit when describing deployments.

Each call owns provider reference caches and conversation state. Account credentials, refresh caches and one prepared profile store are shared within the service scope. Pure shared service descriptions are distinct from factories that allocate call-local resources. Accepted profile operations drain their scoped worker. Accepted note/message sends have independent bounded lifetimes and drain before their underlying sender and ledger close. Async cancellation is never translated into a retry. Unknown provider outcomes remain durable and non-retryable.

## Persistence

SQLite is used for receipts and positions, not as a conversational database: Gmail sends, Calendar creates, SMS notifications and the PBX event-feed cursor. The schemas and request fingerprints preserve compatibility with the Python version. A reservation is written before a side effect. A duplicate can return the existing receipt without repeating the side effect. Calendar checks an existing receipt before scanning conflicts.

The saved profile remains a private JSON document with atomic replacement. Typed read and update capabilities share one prepared store; reads obtain a coherent descriptor snapshot without creating a mutation lock or reading the clock. Context and Windguru consume typed profile state directly. Its existing directory/file ownership, mode and link restrictions are preserved. Saved values are reference facts, never permissions. Note audio comes from the existing Asterisk mixed-call WAV. Notes select an excerpt rather than starting a second recorder; only the VAD remainder and bounded transcript history are retained in memory.

## Deployment boundary

GHCup supplies GHC and Stack; Stack drives the build. Cabal is Stack's internal build backend. The small Setup hook resolves native library paths at configure time and introduces no runtime dependency on the build checkout.

The release contains the executable, global configuration and native license notices; the private user file and existing state are provisioned separately. Asterisk remains an external service. Native phone-number/VAD code and ICU codecs are linked into the executable. The system still supplies the standard C/C++ libraries, zlib, GMP and libsoxr; owner-note MP3 encoding requires ffmpeg. Verify the final executable with `ldd` on the target. Matching architecture alone is insufficient to prove compatibility.

The shared HTTP manager disables automatic request replay and environment proxies. Configured mutation outcomes distinguish authentication failure, definite rejection and an unknown accepted-send outcome. Typed request bodies and derived JSON codecs keep serialization at shared boundaries.

## Shared functionality

- `HTTP` owns TLS managers, authentication headers, JSON/form bodies, response bounds and mutation uncertainty. Its shared `requestJSON` runner accepts a pure response parser. `Provider.get` and `Provider.post` construct requests purely; provider parsers use `JSON.decodeValue` directly. `boundedMap` runs bounded concurrent effects.
- `Effects` centralizes failure dispatch, pure result mapping, short-circuit action folds, operational exception boundaries, result-aware resource cleanup and deadline failures. These functions have explicit `IO` types; their supplied parsers and state decisions stay pure. `Concurrent` commits pure MVar transitions and supplies the shared frame-pacing calculation.
- `JSON` derives typed record codecs and centralizes strict argument parsing and optional event-field extraction. Protocol envelopes remain in the protocol/runtime boundary; provider modules describe typed data.
- `Cache` supplies successful-once and bounded memoization functions. Credentials are cached at account scope; provider references and owner addresses are cached at call scope. Contacts warm-up reuses the same successful-once cache, including its serialized initialization and retry-after-failure behavior.
- `Storage` owns private-file and atomic-write mechanics. `Ledger` is reused by Gmail, Calendar and owner notes; `SMS` retains its distinct existing receipt schema.
- `MIME`, `MailContent` and `Encoder` share message construction, standard charset conversion, HTML extraction and MP3 encoding between ordinary mail and owner notes.

Tool payloads are decoded at their input boundary into typed commands and prepared requests. Calendar dates, Maps requests, Windguru windows and recording intervals are prepared once and reused by the effectful steps. Help is a pure value computed when the call's available tools are assembled; it has no internal tool-name or unused-argument checks. Internal direct-call modes, hangup reasons and end-call reasons are algebraic data types.

Pure functions handle audio framing, stream preparation, transcript reduction, PIN state transitions, message admission, cursor positions and reconnect outcomes. HTTP URL preparation and response projection are pure too. OAuth refresh and consent responses are validated and projected using explicitly supplied timestamps; their coordinators retain the original request/clock order. Provider results, Calendar pagination cursors, conversation history and note rejection replies are computed from explicit values. AudioSocket identity extraction and managed-media packet decisions consume already read packets. SMS retry decisions consume database rows inside the existing transaction; profile projections consume typed snapshots.

The surrounding I/O coordinators fetch data, read clocks/state and commit the selected changes. Transcript transformations indicate when no update is needed, so unrelated events do not write shared state or wake waiters. Generic codecs and shared result constructors handle routine JSON envelopes.

Functional state transitions return new values and decisions. Their small I/O callers commit the state or execute the selected operation. Shared deadline helpers take the complete failure value so uncertainty is preserved. Provider-specific mutation receipts, timestamped forecast caches and timeout recovery retain their own pure interpretation. Library exceptions are converted to typed failures at operational boundaries; asynchronous cancellation retains its resource-cleanup meaning.

The deployment Makefile stages immutable releases and activates one through a removable systemd override. The existing Python unit, environment, virtual environment and shared state remain available for rollback. Installation and activation are separate commands; neither modifies Asterisk configuration. See [the deployment pipeline](deployment.md).
