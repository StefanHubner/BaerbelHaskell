# Migration evidence

The Python baseline is commit `3d0819a` on `refactor/consolidate-codebase` in the sibling `baerbel` repository. Its 860 tests passed before porting. Runtime Python source has not been changed by the port. The Haskell implementation lives in a separate repository.

Local validation on 2026-11-23: `make test` passed all 349 examples with no compiler warnings. Coverage includes native and pure differential fixtures, provider request/result comparisons, cancellation, permissions and persistence. The final run caught and fixed a shared JSON regression: explicit null fields must remain present, so the nullable wrapper keeps a tiny custom instance rather than deriving field-omission behavior from `Maybe`. The existing Maps and Calendar fixtures now pass unchanged. See [the provider parity matrix](provider-parity.md).

`make check USER_CONFIG=/tmp/baerbel-local-user.json` also passed: HLint reported no hints, HLS checked 91 files with zero failures, and the executable passed its offline configuration/native audio check. That private temporary file used a deliberately fake key; provider access was not tested.

Shell syntax and ShellCheck passed for the scripts. Local archive staging was exercised twice; six mocked activation/rollback scenarios verified success only when both assistant and Asterisk services are active. No real service changes were made by these checks.

This is a compiling migration checkpoint, not a claim of complete production-call parity. Further purity/style consolidation remains, especially reducing pure work inside provider coordinators. Live owner/public calls, PIN behavior, incoming screening, native forwarding and real provider actions still need operational verification before replacing Python.

Production remains the original Python installation. Read-only inspection on 2026-11-23 found `phone-assistant` and `asterisk` active. The target is x86-64 with glibc 2.39; the build host is x86-64 with glibc 2.35. Noninteractive privileged installation is unavailable: `sudo -n true` reports that a password is required. No service was restarted and no Asterisk configuration was changed. Privileged Asterisk configuration contents could not be re-inspected without sudo. No real call, SMS, email or OAuth grant was made by the Haskell tests.
