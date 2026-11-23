# Deployment and Python transition

The Haskell implementation is a release candidate. Local tests and `--check` do not establish production-call parity. This workflow stages a release first and activates it only through a separate command. It does not change Asterisk configuration or initiate a call, SMS, email or OAuth grant. Activation starts the normal application, whose enabled integrations can make network requests.

The existing Python installation, virtual environment, `/etc/phone-assistant/assistant.env`, original systemd unit and its existing overrides remain in place. Shared credentials and runtime state stay at their existing absolute paths outside this repository. There must be only one running implementation using the listeners and state files.

## Build and validate locally

Use the GHCup compiler selected by `stack.yaml`. `make prepare` places `.stack-work`, `.native`, `.tools` and `release` in `~/.cache/baerbel-hs/` and leaves links in the checkout. Build products are outside Dropbox; source, fixtures and configuration examples remain in Git. [Dropbox documents that external symlink targets are not synced](https://help.dropbox.com/sync/symlinks).

```sh
make test
make check USER_CONFIG=/absolute/private/user.json
make package
sha256sum release/baerbel-linux-x86_64.tar.gz
make -n install TARGET=stefan@miniserver INSTALL_PREFIX=/opt/baerbel-hs
```

`make test` builds missing native libraries and runs all local tests. Build, check and package keep the test component enabled so Stack can reuse the same compiled library; only `make test` runs the tests. `make check` runs HLint and HLS, then validates the supplied configuration, paths and native audio dependencies without provider requests. Install the matching HLS and `.tools/hlint` first, as described in the README. Use an isolated private test configuration for local path checks; do not put production credentials or user data in this checkout. Package only the source revision whose tests and diagnostics passed. The archive contains executables, global configuration, an example user configuration, licenses and deployment templates; it excludes credentials and state.

The first native build needs the prerequisites in [native/README.md](../native/README.md). Run the compiler, HLint and HLS checks in [README.md](../README.md#development-checks) before release. `make -n` prints commands without uploading or restarting anything.

## Prepare the miniserver

Set `TARGET` to its SSH destination. The examples use an SSH alias; an address can also be supplied. Set one dedicated installation prefix and use the same value for every step. The existing service must be `phone-assistant.service`; its configured service user is reused for the pre-activation check. Remote staging and activation need `sudo`, which may prompt interactively.

Before transition, record the current service configuration and confirm Python and Asterisk are healthy:

```sh
ssh stefan@miniserver 'systemctl is-active phone-assistant asterisk; systemctl show phone-assistant --property=ExecStart --property=User'
ssh -t stefan@miniserver 'sudo systemctl cat phone-assistant; sudo asterisk -rx "core show channels concise"'
```

Keep any saved configuration record private because local overrides may contain secrets. Preserve the Python application and virtual environment in their current locations; do not uninstall them. Back up existing state and credentials using the established private backup process. Rollback below keeps current state, so it does not discard receipts or replay already accepted actions.

Create `/etc/phone-assistant/user.json` from the current Python environment file. This one-time command parses dotenv data using the existing Python environment; it does not execute that file as shell code. It refuses to overwrite an existing user JSON file. Run it on the miniserver:

```sh
sudo /home/stefan/.venvs/assistant/bin/python - <<'PY'
import json
import os
import pwd
from dotenv import dotenv_values

owner = pwd.getpwnam("stefan")
values = dotenv_values("/etc/phone-assistant/assistant.env")
settings = {key: value for key, value in values.items() if value is not None}
with open("/etc/phone-assistant/user.json", "x", encoding="utf-8") as stream:
    os.fchmod(stream.fileno(), 0o600)
    os.fchown(stream.fileno(), owner.pw_uid, owner.pw_gid)
    json.dump({"environment": settings}, stream, indent=2)
    stream.write("\n")
PY
```

Review the private JSON against the previous effective settings and `config/global.json`: preserve credential, profile, receipt, event-cursor, recording and log paths; owner/public listener ports; owner identities; feature flags; provider models and voice. Keep all values as strings. Keep state directories and their existing ownership/permissions. The global file supplies defaults and policy; the user file overrides settings through its `environment` object. No provider access is established by converting this file.

## Stage and inspect a release

```sh
make install TARGET=stefan@miniserver INSTALL_PREFIX=/opt/baerbel-hs
ssh stefan@miniserver 'readlink -f /opt/baerbel-hs/staged; ldd /opt/baerbel-hs/staged/bin/phone-assistant-server'
ssh -t stefan@miniserver 'sudo -u stefan /opt/baerbel-hs/staged/bin/phone-assistant-server --check /opt/baerbel-hs/staged/config/global.json /etc/phone-assistant/user.json'
```

`install` transfers the archive and helper, extracts an immutable release under `/opt/baerbel-hs/releases/<archive-sha256>`, and updates the `staged` link. It does not select a running executable or restart a service. Compare the release directory name with the locally printed archive checksum.

Resolve missing libraries before activation. The target needs the libraries named by `ldd`, plus the configured ffmpeg command and existing Asterisk installation. The native build supplies static compatibility archives; inspect the final executable rather than assuming that every dependency is static. A successful build on the workstation is insufficient evidence of binary portability. Do not copy unrelated libraries over system libraries. The target `--check` must succeed as the service user.

The packaged service file shows the intended hardening and paths. The transition helper preserves the existing unit instead of replacing it. Existing logrotate and Asterisk configuration also stay in place; review packaged templates separately if operational changes become necessary.

## Activate explicitly

Schedule a quiet period and verify `core show channels concise` contains no active calls. A restart disconnects active assistant sessions. Then:

```sh
make activate TARGET=stefan@miniserver INSTALL_PREFIX=/opt/baerbel-hs
ssh stefan@miniserver 'journalctl -u phone-assistant --since "5 minutes ago" --no-pager'
```

Activation runs the staged executable's `--check` as the existing service user before changing the service. It writes only `/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf`, selecting that immutable release's executable and global configuration plus `/etc/phone-assistant/user.json`. It reloads systemd, restarts `phone-assistant`, checks that both services are active, and prints the selected executable and PID. Private configuration, application data and the Python unit are untouched.

If a restart fails, use the rollback command below. Process checks are not a call test. Record the deployed archive checksum, effective executable, service status and log evidence separately from behavior actually exercised. Owner/public routing, PIN protection, incoming screening, native forwarding and provider effects need separately authorized real-call verification before claiming production parity.

## Roll back to Python

```sh
make rollback TARGET=stefan@miniserver INSTALL_PREFIX=/opt/baerbel-hs
ssh stefan@miniserver 'journalctl -u phone-assistant --since "5 minutes ago" --no-pager'
```

Rollback removes only the Haskell override, reloads systemd and restarts the original Python unit. It checks both services and prints the effective executable and PID. It leaves Haskell releases and the private user JSON available for investigation. It does not rewind shared state or alter Asterisk. Review the printed `ExecStart` to confirm the preserved Python virtual environment is selected, then record separately any real calls exercised after rollback.

The latest actual verification and deployment status belong in [migration.md](migration.md); the presence of this workflow does not mean installation or activation has occurred.
