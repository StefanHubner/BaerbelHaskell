# Migration evidence

The Python baseline is commit `3d0819a` on `refactor/consolidate-codebase` in the sibling `baerbel` repository. Its 860 tests passed before porting. Runtime Python source has not been changed by the port. The Haskell implementation lives in a separate repository.

Local validation on 2026-11-25: `make test` passed all 364 examples with no compiler warnings. Coverage includes native and pure differential fixtures, provider request/result comparisons, cancellation, permissions and persistence. The cases also cover prepared Calendar requests, Contacts warmup, path preparation and typed profile reads without clock/write/lock effects. New regressions cover definite malformed-URL failures and transcript events that must not request a state write. Existing behavior fixtures pass unchanged. See [the provider parity matrix](provider-parity.md).

The nullable JSON wrapper retains its explicit-null encoding: the earlier checkpoint established that deriving field omission from `Maybe` would change Maps and Calendar outputs. Its small custom instance preserves the required wire representation.

`make test check USER_CONFIG=/tmp/baerbel-pure-results-user.json` also passed: HLint reported no hints, HLS checked 93 files with zero warnings or failures, and the executable passed its offline configuration/native audio check. That private temporary file used a deliberately fake key; provider access was not tested.

Shell syntax and ShellCheck passed for the scripts. Local archive staging was exercised twice; six mocked activation/rollback scenarios verified success only when both assistant and Asterisk services are active. No real service changes were made by these checks.

The initial functional cleanup was completed on 2026-11-25. Provider coordinators now consume typed prepared requests; pure interpretation, audio/frame decisions and state transitions sit outside I/O. Help is prepared once without internal name/unused-argument checks. Profile readers share one typed store, avoid JSON round trips and read snapshots without mutation locks or clocks. Internal native-call modes and hangup reasons use typed commands. Repeated validation of controlled internal parameters has been removed; external data is decoded at its boundary. Application code and test helpers use guards and pattern matching throughout, with no if/then/else expressions.

The subsequent pure-result audit converted 44 existing functions to pure types and removed 13 effect wrappers. Request construction, pagination, response normalization, token interpretation, packet decisions and state/result transformations now consume explicit values. Existing coordinators retain the original effect order, atomic updates and cancellation hooks. See [the source inventory](source-inventory.md) for consistent before/after counts.

Deployment and live-call testing are deferred to the next discussion at the user's request. No remote actions were performed during the cleanup. The existing Python service remains the deployed implementation.

Production remains the original Python installation. Read-only inspection on 2026-11-23 found `phone-assistant` and `asterisk` active. The target is x86-64 with glibc 2.39; the build host is x86-64 with glibc 2.35. On that inspection, `sudo -n true` reported that a password was required. No service was restarted and no Asterisk configuration was changed. Privileged Asterisk configuration contents could not be re-inspected without sudo. No real call, SMS, email or OAuth grant was made by the Haskell tests.
