# Migration evidence

## Provider tool files and shared outcome messages

On 2026-11-25 at 23:51 UTC, the miniserver ran release
`21d9a94492de2717260a9bdf3d01c54cecd4d222a180321ae52f5f6cf467ca14`
from source `27e84ed`. The installed instruction and provider files
matched the release, and SQLite passed its integrity and foreign-key
checks. Bärbel and Asterisk were active with zero restarts after startup.
Asterisk kept the same process during this release. FastAGI checks
exercised both allowed routes and rejected Pro and unknown callers from
the shared route. No real carrier calls were placed during these checks.


## Portable site configuration and instructions

On 2026-11-25 at 23:36 UTC, the miniserver ran release
`1309600e6073028ca464df2d67272c0e3e138a33a36daa402df6ea80783b5157`
from source `8e73e71` after `make activate config`. Bärbel and Asterisk
were active with zero restarts after startup. The installed instructions
matched the release, both private YAML files had mode `0600`, and
FastAGI selected the Standard and Pro accounts. SQLite passed integrity
and foreign-key checks and retained the three Google connections, five
delivery receipts, eight SMS receipts and six event cursors. The test
using stock Asterisk config exercised one initial apply and a zero-write
repeat. These checks exercised the application routing and installer.
Real carrier call evidence below belongs to earlier releases.


Release `49448f75…` from source `4a41916` activated at
2026-11-25T22:58:48Z. The service uses `/etc/phone-assistant/global.yaml`.
The Haskell `asteriskconfig` executable installed the managed configuration
in `/etc/asterisk/phone-assistant/`. The private Asterisk YAML carries the
local handset and Telematica rules. SQLite links the two user types to
their SIP accounts, with the existing Pro state under `stefan`. The
provider and caller numbers are separate fields.

After the repeat `make config`, Bärbel was active at PID 233114 and
Asterisk at PID 232923, both with zero restarts. The former JSON,
`account-routes`, and Telematica installer inputs were archived.
Production FastAGI matched both users, and the active executable passed
its configuration check. The real carrier-call evidence below belongs
to the earlier releases.


The previous user database release was `0dd48e38…` from source `06d2f54`, activated
at 2026-11-25T22:01:36Z. The running Haskell service loaded one private
`global.json` and `users.sqlite3`, both mode `0600`. The former account JSON
was archived. Local checks passed 409 examples, HLint and 160 HLS file
checks. Migration rehearsals preserved memory, link scopes, receipts and
the event cursor. Existing Google grants accepted live refresh requests.
Production FastAGI selected both accounts correctly. PID 229128 owned
all three Bärbel listeners with zero restarts. Asterisk continued on
PID 226605. Real carrier call evidence below retains its original release
scope.

After archival, the server verified the running Haskell command still used
`/etc/phone-assistant/global.json`. Both services kept their existing
PIDs, the executable configuration check passed, and SQLite reported
integrity OK. `users.sqlite3` is the only file in the application state
directory. The obsolete standalone state and Python configuration are now
in the [private archives](deployment.md#archive-legacy-files).

Haskell is active in the existing production service as of 2026-11-25T15:31:08Z. Post-activation process, listener and startup-log checks passed. Later approximately 149-second calls exposed an input-overflow failure; corrections passed local and focused target-provider checks and were activated at 17:18:47Z. Python is preserved for rollback. Real tests verified the owner Gmail path and known-caller forwarding/ringing on Haskell. The forwarded caller ended before answer and its outcome SMS was accepted; answered bridging and other call paths remain unverified.

The Python baseline is commit `3d0819a` on `refactor/consolidate-codebase` in the sibling `baerbel` repository. Its 860 tests passed before porting. Runtime Python source has not been changed by the port. The Haskell implementation lives in a separate repository.

Earlier local validation on 2026-11-25: `make test` passed all 384 examples with no compiler warnings. Coverage includes native and pure differential fixtures, provider request/result comparisons, cancellation, permissions and persistence. The cases also cover prepared Calendar requests, Contacts warmup, path preparation and typed profile reads without clock/write/lock effects. New regressions cover definite malformed-URL failures and transcript events that must not request a state write. Existing behavior fixtures pass unchanged. See [the provider parity matrix](provider-parity.md).

The nullable JSON wrapper retains its explicit-null encoding: the earlier checkpoint established that deriving field omission from `Maybe` would change Maps and Calendar outputs. Its small custom instance preserves the required wire representation.

`make test check USER_CONFIG=/tmp/baerbel-plumbing-user.json` also passed: HLint reported no hints, HLS checked 139 files with zero warnings or failures, and the executable passed its offline configuration/native audio check. That private temporary file used a deliberately fake key; provider access was not tested.

Shell syntax and ShellCheck passed for the scripts. Local archive staging was exercised twice. The expanded deployment harness passed 16 scenarios covering activation, rollback, startup failure, an unexpected executable, process replacement, missing process metadata and dirty-source packaging. Activation verifies the exact staged executable and a stable process after a bounded startup wait; rollback reports the restored executable. Both require assistant and Asterisk to be active. These are installation checks, not telephone-call tests; no real service changes were made.

The initial functional cleanup was completed on 2026-11-25. Provider coordinators now consume typed prepared requests; pure interpretation, audio/frame decisions and state transitions sit outside I/O. Help is prepared once without internal name/unused-argument checks. Profile readers share one typed store, avoid JSON round trips and read snapshots without mutation locks or clocks. Internal native-call modes and hangup reasons use typed commands. Repeated validation of controlled internal parameters has been removed; external data is decoded at its boundary. Application code and test helpers use guards and pattern matching throughout, with no if/then/else expressions.

The subsequent pure-result audit converted 44 existing functions to pure types and removed 13 effect wrappers. Request construction, pagination, response normalization, token interpretation, packet decisions and state/result transformations now consume explicit values. Existing coordinators retain the original effect order, atomic updates and cancellation hooks. See [the source inventory](source-inventory.md) for consistent before/after counts.

The shared-plumbing consolidation centralizes request execution with pure response parsers, failure dispatch, short-circuit action folds, operational exception handling, deadlines and resource cleanup. Pure JSON field extraction and state transformations are reused across providers and runtime modules; shared clock conversion and frame pacing preserve their original timing calculations. Contacts warm-up now uses the existing successful-once cache. Regression cases cover skipped effects after failure, retained delivery uncertainty, failed acquisition and cleanup during cancellation. Existing provider and runtime fixtures remain unchanged.

The follow-up effect audit inspected every production module and separated dialing variables/state transitions, startup classification, announcement readiness, Gmail search preparation/results, Maps reference lookup, profile write planning and SMS request/receipt construction from their effectful callers. Mail decoder selection and rendering preparation are pure, including valid UTF-8 decoding. Native converters remain for malformed and legacy encodings. A differential check of 233,792 byte sequences found zero body or strict-header output differences from the previous codecs. The new regressions cover no-write profile updates, unknown-reference effect suppression, startup outcomes, exact quiet-period boundaries and Unicode fallback grouping. The full 378-test run, HLint, HLS across 95 files and offline configuration/native audio check passed with zero compiler or language-server warnings.

The module separation places domain values, protocol types, parsing, calculations and decisions under `Baerbel.Core.*`; effect capabilities, handles and execution live under `Baerbel.Runtime.*`. Shared configuration, failure and request types no longer require Core to depend on runtime modules. Original paths were removed and all callers/tests were updated. Six architecture regressions enforce the recursive source/import/type boundary, and missing top-level signatures are compiler errors. The existing behavior fixtures are unchanged. The global and project agent instructions record the boundary for future work. The complete Stack run passed all 384 examples; HLint reported no hints, HLS checked 139 files with zero warnings or failures, and offline configuration/native audio checks passed. The regenerated SVG and PNG were inspected for readability and clipping.

Phone-number validation now runs in the Functional Core against maintained numbering metadata compiled at startup. Differential checks covered 1,236,669 inputs with zero mismatches against the previous implementation; all 864 original fixtures and 45 focused telephony tests passed. The phone-number FFI, libphonenumber, protobuf and Abseil build dependencies were removed. ICU has its own reproducible static build for mail decoding; WebRTC VAD and libsoxr retain their actual audio boundaries.

The final real local provider, audio and conversation suite passed all 18 checks, with zero failures and zero disabled integrations. It verified bounded provider finalization and the corrected rapid-keypad PIN handoff: a successful local verification instruction reaches Live before the original pending tool is released, so stale retry speech cannot hide its result. Exactly one lookup resumed. The final Stack suite passed 394 examples; compiler checks, HLint, HLS across 157 files, offline configuration/native checks, ShellCheck and strict metadata-utility typing passed without diagnostics. See [the live-check record](live-checks.md) for coverage, measurements and the remaining actual-call boundary.

The separately authorised delivery batch sent one synthetic voice email to the Gmail profile's own address. Its exact returned message ID was found in the inbox; the retrieved audio/mpeg attachment decoded successfully to 6.192 seconds of audible PCM. No second email was sent. The temporary private calendar event was created, read back and deleted. The first cleanup assertion incorrectly expected only HTTP 404/410: a bounded read-only recovery confirmed the exact test-event ID returned HTTP 200 with status `cancelled`, which [Google's Events reference](https://developers.google.com/workspace/calendar/api/v3/reference/events) documents as a deleted-event record. The corrected calendar-only repeat passed in 1.484 seconds: one private 120-second event with no attendees was created and read back, DELETE returned 204, and the exact-ID GET returned a cancelled tombstone. Nine local cleanup scenarios also pass, including cancellation, uncertain creation and rejection of mismatched or still-confirmed records. No other calendar entries were changed.

Before cutover, production was the original Python installation. Read-only inspection of `192.168.50.83` at 2026-11-25T14:40:47Z found `phone-assistant` and `asterisk` active. The target is x86-64 with glibc 2.39; the build host is x86-64 with glibc 2.35. Non-interactive sudo reported that a password was required. No service was restarted and no Asterisk configuration was changed; privileged configuration contents could not be re-inspected without sudo. At that inspection, Haskell activation and real telephone-call verification were outstanding. The Python package and environment are preserved for rollback; the documented cutover replaces the executable in the existing service rather than running overlapping assistants.

At 2026-11-25T15:27:30Z, the Haskell release built from `23946cb` was installed and checked on the miniserver under `/home/stefan/apps/baerbel/releases/421e7cad8704c7209d4b9137b5fc9e71d455306bfa4f907583c3786c243bdeb6`; `/home/stefan/apps/baerbel/staged` selects it. Target library resolution and the actual executable's configuration/native-audio checks passed. The prepared private configuration is owned by `stefan` with mode `0600`. Migration preserves literal dotenv values and the existing Python SMS fallback from empty SMS-specific credentials to the A&A SIP credentials; those alias rules live in the shared global configuration. No credential values were printed.

The fixed-host deployment uses `make install`, `make activate` and `make rollback` without arguments. Thirty-two local deployment scenarios and eight configuration-conversion regressions passed; ShellCheck and strict helper typing passed. The workstation's fixed private configuration also passed its offline check, using a local profile path. Haskell runtime code is unchanged from the preceding 394-example and 18-live-check result.

The first noninteractive activation attempt stopped at `sudo: a password is required`, leaving Python PID 146123 and Asterisk active. The user then completed the privileged activation. The new Haskell process started at 2026-11-25T15:31:08Z; inspection at 15:31:41Z and the subsequent health check confirmed PID 214856 remained active/running with `NRestarts=0`. Its `/proc` executable matches the installed release above, and the old Python PID is absent. Haskell owns loopback ports 9092 (owner), 9093 (public) and 4573 (FastAGI); Asterisk remains active. The journal and application log show the expected service transition and startup, with no warning/error entries since activation. The installed private configuration is owned by `stefan` with mode `0600`, and the service override is owned by root with mode `0644`.

The startup checks did not initiate a telephone call. The deployment helper does not modify Asterisk configuration; independent inspection of the loaded dialplan could not be completed because noninteractive sudo is unavailable and the existing AMI account rejected both GetConfig and the specifically scoped ShowDialPlan request. AMI authentication itself succeeded. No permissions were broadened. `/home/stefan/apps/assistant`, the Python virtual environment and shared state remain preserved for rollback.

A real owner Gmail call on 2026-11-25, approximately 15:58:12–15:59:57 UTC, was confirmed by the user and verified against the service journal. All 185 call entries identify PID 214856 and the deployed Haskell executable in release `421e7cad8704c7209d4b9137b5fc9e71d455306bfa4f907583c3786c243bdeb6`; the old Python process remains absent and the service still reports zero restarts. Two `gmail_search` actions with distinct tool-call IDs returned `ok`, two delegation events were recorded, four backend responses completed with none failed or incomplete, and session closure recorded `finalized=true`. Private PIN protection was configured and its keypad prompt was observed; no private PIN or transcript values are included here. No warning/error journal entries were found for the call. [The live-check record](live-checks.md#real-owner-gmail-call) records timings without personal email contents or phone numbers. This call verifies the owner Gmail path; the later incoming-call evidence is recorded below.

A second real test on 2026-11-25 exercised a known external caller's forwarding/ringing, which the user reported behaved as expected. At 16:08:21 UTC, Haskell PID 214856 in the same installed release logged `incoming_classified` with `caller_class=known`. The PBX `incoming-v1` outcome ended at 16:08:40Z with reason `CANCEL` and `answered=0`: the caller hung up before an answer. At 16:08:42 UTC, Haskell logged `incoming_message_delivery` with status `sent`, confirming acceptance of the outcome SMS. No error events were found; Haskell remained at zero restarts and Asterisk remained active. This verifies known-caller forwarding/ringing and the accepted outcome notification, not an answered bridge or an owner outbound/direct call. Unknown-caller screening, home-phone behavior and in-call voice notes remain unverified in real calls.

Later production calls on 2026-11-25 ended at approximately 16:14:13Z and 16:17:56Z with `live_input_overflow`, after roughly 149 seconds and without an `end_call` action. The service process remained PID 214856 with zero restarts. Windguru also returned two forecast errors before two successful results; its saved-set decoder issue was subsequently reproduced and corrected separately. Maps lookups/routes succeeded, but transcript-event response intervals of 8.207 and 14.918 seconds exposed slow multi-step backend responses while individual tool execution times stayed below one second. The deployed model, `high` reasoning effort and output-token limit match the preserved Python configuration. [The incident record](live-checks.md#production-incidents-under-investigation) separates these observations from earlier successful checks.

The corrective source now computes absolute audio deadlines in `Core.Concurrent`, performs clock waits in Runtime with epoch/pause suppression preserved, and logs tool failure codes without arguments or results. The final `make check` passed all 406 examples, HLint reported zero hints, HLS checked 159 files with zero failures, and offline configuration/native checks passed. The expanded target suite initially completed with 17 passes and two failures: the saved-set decoder and a false soak assertion. Both subsequently passed focused reruns. Interruption, PIN, owner hangup and cancellation passed alongside provider/audio checks in the full run; the combined evidence is not a single all-green full-suite rerun.

The saved-set correction accepts arrays covering all timestamps, ignores trailing readings and still rejects short arrays. Target responses had 144 timestamps and 150 readings; the original exact-length check rejected 15 of 16 members. Direct target checks now return 15 available members for the current 6-hour, tomorrow 24-hour and multiday 72-hour windows, with 90 and 360 points for the shorter windows and 480 points after multiday compaction. One independent provider API error remains unexplained; no access restriction is inferred. The initial target soak stayed alive for 210 seconds, answered after 190 seconds and finalized, but its assertion wrongly counted variable-sized resampler chunks as fixed frames. The corrected focused target soak passed: 210,005.713 ms wall time, 209,985.167 ms input PCM, a 20.546 ms deficit and 420 ms maximum output queue. Its late follow-up delivered speech after 1,422.239 ms, provider finalization was acknowledged, and shutdown took 809.678 ms. Telephone transport was simulated; a real Asterisk call after activation remains distinct. The corrected release was activated at 2026-11-25T17:18:47Z; [the live-check record](live-checks.md#production-incidents-under-investigation) contains the scoped evidence.


## Corrective release activated

The audio-pacing and Windguru corrections are committed as `21d47c7b6b8aaeda2f3b6efff581ab5d55ce89f4` on `refactor/live-deployment-ready` and pushed over SSH. Release `8233b7061fc04531b9363f0dd9ab83354a3a73b63206ad15dcf59c0a8b6cce4f` was installed under `/home/stefan/apps/baerbel/releases/` and selected by `staged` at that time. Its target dependency, configuration and native-audio checks passed.

The first activation attempt stopped because sudo required a password. Activation subsequently completed at **2026-11-25T17:18:47Z**. Read-only verification confirmed `phone-assistant.service` active/running at **PID 219249**, **zero restarts**, and `/proc/219249/exe` pointing to the exact corrective release above; Asterisk was active. That corrective release served calls until the subsequent activation below. A real Asterisk call after this activation remains distinct from the successful target checks with simulated telephone transport.

## Live-only release activated

Source `95bae18204b5da142c0f00e4f6166790d9b65a40` separates shared phone reference metadata from application settings, removes the unused alternate STT/chat/TTS pipeline and imported rule snapshots, and moves synthetic TTS to development-only configuration. It selects backend `gpt-6.2-sol` with `low` reasoning and `priority` service tier. Local validation passed: **402 tests with zero failures** (including all 864 telephone fixtures), HLint with zero hints, HLS on **157 files with zero failures**, and offline configuration/native checks. Phone metadata is unchanged and regenerates identically; the permitted-number policy is unchanged. The archive excludes the development checker and its configuration.

Release `559d2cdf111aa2fddc3d8d1ceb6c29b0918838567a25f2a1ebf20b236ab371c1` was activated at **2026-11-25T19:48:47Z**. Verification confirmed `phone-assistant.service` active/running at **PID 221942**, **zero restarts**, and its `/proc` executable matching the exact staged release. PID 221942 owns loopback ports **9092**, **9093** and **4573**; Asterisk is active. Effective backend settings are `gpt-6.2-sol`, `low`, `priority`. The installed private file remains mode **0600** with no obsolete TTS/STT/voice-mode keys, and the separate phone metadata file is present.

A focused actual Live-provider check on the miniserver used the staged configuration and a synthetic tool. Two backend responses completed with `gpt-6.2-sol`, `low` reasoning and reported tier `fast` for requested `priority`; the exact tool round trip succeeded, no provider errors occurred, and provider finalization was acknowledged. The first check failed only because its assertion required the returned spelling `priority`; the corrected assertion accepted the observed `fast` spelling and the repeat passed. This was not a latency benchmark or a real Asterisk call. The runtime remains single-user, and shared versus separate telephone numbers and future tenant routing remain undecided.
