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

`config/global.json` contains shared defaults, policy, tool schemas and prompts. Copy `config/user.example.json` to a private file outside Git, set its `environment` entries, and restrict it to its service owner. The example contains no working credential. Provider credentials and state-file paths are referenced from that one user configuration.

```sh
stack exec phone-assistant-server -- --check config/global.json /private/user.json
stack exec phone-assistant-server -- config/global.json /private/user.json
```

`--check` validates local configuration and native dependencies without making provider requests. The server form can make provider requests and exposes the configured local telephony listeners. Do not start it on the running service's ports until migration checks and deployment prerequisites are satisfied.

The original Python service remains deployed. Local provider checks and deployment instructions do not activate Haskell; see the migration and live-verification records for the behavior actually exercised.

## Development checks

The Stack cradle in `hie.yaml` selects the library, runtime commands, live-check executable, tests and native build hook. Use the GHCup HLS matching the selected GHC. Compiler warnings are enabled in `package.yaml`; no warning categories are suppressed.

```sh
make prepare
stack install hlint --local-bin-path .tools
make check USER_CONFIG=/private/user.json
```

`make check` includes the test suite, HLint, HLS and the offline executable check. To exercise configured providers and synthetic speech through the actual Haskell runtime, run:

```sh
make live-check USER_CONFIG=/private/user.json
```

This command makes real API requests, including OAuth refresh and billed model/audio requests. It reports redacted results, timings and cache assertions; it does not activate a service, send email/SMS, or create calendar events. Disabled integrations are reported separately. See [live-checks.md](docs/live-checks.md) for coverage and measured outcomes; a local live check does not verify an Asterisk call.

For an explicitly requested end-to-end delivery check, run `make live-delivery-check USER_CONFIG=/private/user.json`. It sends one labelled self-email with synthetic audio and creates, reads and deletes one private calendar test event. Delivery and cleanup are reported separately.

## Release candidate

After committing the validated source, `make package` creates `release/baerbel-linux-x86_64.tar.gz`. It contains both compiled commands, global configuration, a private-user-file example, native license notices and reviewable systemd/logrotate files. Packaging refuses uncommitted source changes. It does not install or restart anything. The existing Asterisk deployment is maintained separately.

The server command uses the global and private user files shown above. The preserved authorization command is `phone-assistant-gmail-auth GLOBAL_CONFIG --client-secrets FILE`; its existing integration selection, browser, port and timeout options remain available. Authorization performs a real consent flow and is separate from offline validation.

Local packaging and archive staging have been exercised; see [migration.md](docs/migration.md) for that evidence. Rebuild the archive from the source revision whose checks passed. The standalone service template uses `/opt/baerbel`; the transition instructions instead stage under `/opt/baerbel-hs` and change the existing `phone-assistant.service` through one override. Do not install a second service. Both use `/etc/phone-assistant/user.json`. Keep existing credential and receipt paths, and preserve the Python application and virtual environment for rollback.

Follow [deployment.md](docs/deployment.md) to stage, inspect, activate during a period with no active calls, and restore Python if needed. Staging does not restart anything. Activation and rollback restart the same service; no Python package is uninstalled. The target still supplies Asterisk, libsoxr and ffmpeg.
