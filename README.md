# Bärbel in Haskell

A functional port of the preserved Python telephone assistant, built with GHCup and Stack. See [the module overview](docs/architecture.md), [the editable diagram](architecture/README.md), [current migration evidence](docs/migration.md), and [deployment and Python rollback](docs/deployment.md).

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

The original service remains deployed while parity and operational verification are completed. Refer to the migration record for evidence, rather than inferring deployment status from the presence of a binary or a passing build.

## Development checks

The Stack cradle in `hie.yaml` covers the library, both executables, tests and the native build hook. Use the GHCup HLS matching the selected GHC. Compiler warnings are enabled in `package.yaml`; no warning categories are suppressed.

```sh
make prepare
stack install hlint --local-bin-path .tools
make test
make check USER_CONFIG=/private/user.json
```

## Release candidate

After committing the validated source, `./scripts/package-release.sh` creates `release/baerbel-linux-x86_64.tar.gz`. It contains both compiled commands, global configuration, a private-user-file example, native license notices and reviewable systemd/logrotate files. It does not install or restart anything. The existing Asterisk deployment is maintained separately.

The server command uses the global and private user files shown above. The preserved authorization command is `phone-assistant-gmail-auth GLOBAL_CONFIG --client-secrets FILE`; its existing integration selection, browser, port and timeout options remain available. Authorization performs a real consent flow and is separate from offline validation.

The standalone service template uses `/opt/baerbel`; the documented transition pipeline instead stages releases under `/opt/baerbel-hs` and overrides the existing service. Both use `/etc/phone-assistant/user.json`. Keep existing credential and receipt paths when transferring settings. The archive excludes credentials and runtime state. The target also needs its existing Asterisk, libsoxr and ffmpeg installation; the migration record states which checks actually ran and why installation is still pending.
