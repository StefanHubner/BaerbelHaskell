# Bärbel in Haskell

A functional port of the preserved Python telephone assistant, built with GHCup and Stack. See [the module overview](docs/architecture.md), [the editable diagram](architecture/README.md), [current migration evidence](docs/migration.md), [live verification](docs/live-checks.md), and [deployment and Python rollback](docs/deployment.md).

## Build

Use the GHC version selected by `stack.yaml` through GHCup. The Make targets prepare build directories outside Dropbox, build missing native compatibility libraries and invoke Stack:

```sh
make test
make build
```

The build uses the tested LTS package set. Package requirements specify minimum supported versions. Native source/metadata versions and checksums preserve the reference implementation's behavior; see [native/README.md](native/README.md).

## Configuration and execution

`config/global.json` contains shared defaults, policy, tool schemas and prompts. Local checks use `~/.config/baerbel/user.json`, prepared from the existing Python configuration and kept private outside Git. Provider credentials and state-file paths are referenced from that one user configuration; `config/user.example.json` documents its format without working credentials.

```sh
stack exec phone-assistant-server -- --check config/global.json ~/.config/baerbel/user.json
```

`--check` validates local configuration and native dependencies without making provider requests. Deployment uses the existing service and fixed server paths described below.

Haskell now runs in the existing `phone-assistant.service` on the miniserver. The Python application is preserved for rollback. Process, listener and startup-log checks passed. Real tests verified the owner Gmail path and known-caller forwarding/ringing, followed by caller hangup and an accepted outcome SMS. Answered bridging and other call paths remain unverified. See the migration and live-verification records for the behavior actually exercised.

## Development checks

The Stack cradle in `hie.yaml` selects the library, runtime commands, live-check executable, tests and native build hook. Use the GHCup HLS matching the selected GHC. Compiler warnings are enabled in `package.yaml`; no warning categories are suppressed.

```sh
make prepare
stack install hlint --local-bin-path .tools
make check
```

`make check` includes the test suite, HLint, HLS and the offline executable check. To exercise configured providers and synthetic speech through the actual Haskell runtime, run:

```sh
make live-check
```

This command makes real API requests, including OAuth refresh and billed model/audio requests. It reports redacted results, timings and cache assertions; it does not activate a service, send email/SMS, or create calendar events. Disabled integrations are reported separately. See [live-checks.md](docs/live-checks.md) for coverage and measured outcomes; a local live check does not verify an Asterisk call.

For an explicitly requested end-to-end delivery check, run `make live-delivery-check`. It sends one labelled self-email with synthetic audio and creates, reads and deletes one private calendar test event. Delivery and cleanup are reported separately.

## Release and deployment

After committing the validated source, `make package` creates `release/baerbel-linux-x86_64.tar.gz`. It contains both compiled commands, global configuration, a private-user-file example, native license notices and reviewable systemd/logrotate files. Packaging refuses uncommitted source changes. It does not install or restart anything. The existing Asterisk deployment is maintained separately.

The server command uses the global and private user files shown above. The preserved authorization command is `phone-assistant-gmail-auth GLOBAL_CONFIG --client-secrets FILE`; its existing integration selection, browser, port and timeout options remain available. Authorization performs a real consent flow and is separate from offline validation.

Deployment is fixed to the existing SSH account `stefan@192.168.50.83`; no Make variables are required:

```sh
make install
make activate
```

`make install` installs the existing release under `/home/stefan/apps/baerbel`, prepares the private configuration from the server's current settings and runs target library/configuration checks, without sudo or restarting anything. Its upload and configuration workspace is `/home/stefan/.cache/baerbel-hs/deploy`. `make activate` is the privileged step: it rejects active calls, installs the private configuration in `/etc/phone-assistant` and switches the existing `phone-assistant.service` through one override. It verifies the running executable and service stability.

`make rollback` removes that override and restores the same Python service. The Python application in `/home/stefan/apps/assistant`, its virtual environment and shared state are preserved; no second service is installed. The server supplies Asterisk, libsoxr and ffmpeg. See [deployment.md](docs/deployment.md) for the workflow and [migration.md](docs/migration.md) for what has actually been exercised. Installation, target checks and activation passed on 2026-11-25. The Haskell process is active with zero restarts, its three loopback listeners are bound, Asterisk is active and the old Python process has exited. Real owner Gmail and known-caller forwarding/ringing checks passed; the forwarded caller ended before answer. Answered bridging and other call paths still need verification.

The audio-pacing and Windguru corrections in `21d47c7` are installed and checked on the miniserver. Activation requires sudo authentication; run `make activate` from this checkout. See [the release record](docs/migration.md#corrective-release-staged).
