# Deployment and Python transition

The Haskell implementation is a release candidate; the existing Python service remains deployed. These instructions stage a release and then activate it with a separate command. No activation is requested by running local checks or packaging. The scripts do not change Asterisk configuration or initiate a telephone call. Activation starts the normal application, including its enabled provider and notification workers.

The existing Python installation, virtual environment, `/etc/phone-assistant/assistant.env`, original systemd unit and its existing overrides remain in place. Shared credentials and runtime state stay at their existing absolute paths outside this repository. There must be only one running implementation using the listeners and state files.

## Build and validate locally

Use the GHCup compiler selected by `stack.yaml`. `make prepare` places `.stack-work`, `.native`, `.tools` and `release` in `~/.cache/baerbel-hs/` and leaves links in the checkout. Build products are outside Dropbox; source, fixtures and configuration examples remain in Git. [Dropbox documents that external symlink targets are not synced](https://help.dropbox.com/sync/symlinks).

```sh
make check USER_CONFIG=/absolute/private/user.json
make live-check USER_CONFIG=/absolute/private/user.json
make live-delivery-check USER_CONFIG=/absolute/private/user.json
make package
sha256sum release/baerbel-linux-x86_64.tar.gz
make -n install TARGET=stefan@192.168.50.83 INSTALL_PREFIX=/opt/baerbel-hs
```

`make check` builds missing native libraries and runs the test suite, HLint, HLS and the executable's offline configuration/native check. Install the matching HLS and `.tools/hlint` first, as described in the README. `make live-check` separately runs the Haskell live harness against configured providers and synthetic speech, with bounded requests and redacted timing/cache results. It refreshes OAuth tokens and incurs model/audio usage, but does not send email/SMS, create calendar events, bind the service listeners or activate a service. Disabled integrations are explicit in its output. See [live-checks.md](live-checks.md) for actual results and remaining gaps.

The separate `live-delivery-check` command performs the requested self-email/attachment and temporary private calendar-event checks; each invocation sends a new labelled email and verifies event cleanup.

Use a private JSON configuration outside the checkout. Package the same committed source revision whose checks passed: `BUILD.txt` records Git HEAD, compiler and platform details, while the archive checksum identifies the actual artifact. The package script rejects tracked changes and nonignored untracked files before building. Generated artifacts in ignored directories do not prevent packaging. The archive contains the server and authorization commands, global configuration, a user-file example, licenses and templates; credentials and runtime state are excluded.

Local archive creation and repeated staging have been exercised, as recorded in [migration.md](migration.md). That is evidence for the local packaging path, not installation on the miniserver. Provider checks, target process checks and real Asterisk calls establish different things; none substitutes for the others.

The first native build needs the prerequisites in [native/README.md](../native/README.md). Run the compiler, HLint and HLS checks in [README.md](../README.md#development-checks) before release. `make -n` prints commands without uploading or restarting anything.

## Prepare the miniserver

Set `TARGET` to its SSH destination. The examples use the established address from the Python Makefile; `miniserver` does not resolve on the current workstation. Set one dedicated installation prefix and use the same value for every step. The existing service must be `phone-assistant.service`; its configured service user is reused for the pre-activation check. Remote staging and activation need `sudo`, which may prompt interactively.

Before transition, record the current service configuration and confirm Python and Asterisk are healthy:

```sh
ssh stefan@192.168.50.83 'systemctl is-active phone-assistant asterisk; systemctl show phone-assistant --property=ExecStart --property=User'
ssh -t stefan@192.168.50.83 'sudo systemctl cat phone-assistant; sudo asterisk -rx "core show channels concise"'
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
make install TARGET=stefan@192.168.50.83 INSTALL_PREFIX=/opt/baerbel-hs
ssh stefan@192.168.50.83 'readlink -f /opt/baerbel-hs/staged; ldd /opt/baerbel-hs/staged/bin/phone-assistant-server'
ssh -t stefan@192.168.50.83 'sudo -u stefan /opt/baerbel-hs/staged/bin/phone-assistant-server --check /opt/baerbel-hs/staged/config/global.json /etc/phone-assistant/user.json'
```

`install` transfers the archive and helper, extracts an immutable release under `/opt/baerbel-hs/releases/<archive-sha256>`, and updates the `staged` link. It does not select a running executable or restart a service. Compare the release directory name with the locally printed archive checksum. The default archive is `release/baerbel-linux-x86_64.tar.gz`; `RELEASE_ARCHIVE=/absolute/path.tar.gz` selects a different reviewed archive.

Resolve missing libraries before activation. The target needs the libraries named by `ldd`, plus the configured ffmpeg command and existing Asterisk installation. The native build supplies static compatibility archives; inspect the final executable rather than assuming that every dependency is static. A successful build on the workstation is insufficient evidence of binary portability. Do not copy unrelated libraries over system libraries. The target `--check` must succeed as the service user.

The packaged service file shows the intended hardening and paths. The transition helper preserves the existing unit instead of replacing it. Existing logrotate and Asterisk configuration also stay in place; review packaged templates separately if operational changes become necessary.

## Activate explicitly

For activation or rollback, stop initiating new calls and verify `core show channels concise` contains no active calls immediately before the restart. This is an operator step; the helper does not enforce it. Then activate the staged release:

```sh
make activate TARGET=stefan@192.168.50.83 INSTALL_PREFIX=/opt/baerbel-hs
ssh stefan@192.168.50.83 'systemctl show phone-assistant --property=ExecStart --property=MainPID; journalctl -u phone-assistant --since "5 minutes ago" --no-pager'
```

Activation runs the staged executable's `--check` as the existing service user before changing the service. It writes only `/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf`, selecting that immutable release's executable and global configuration plus `/etc/phone-assistant/user.json`. It reloads systemd and restarts the same `phone-assistant` unit: systemd stops Python before starting Haskell, so there is no second service sharing its ports or state. The helper checks that the assistant and Asterisk are active, verifies `/proc/MainPID/exe` is the staged binary, waits three seconds, and repeats the checks with the same PID. It prints the actual executable, `ExecStart` and `MainPID`. Python files, its virtual environment, the original unit, private configuration and application data are retained. Do not also enable the packaged standalone unit.

If startup fails, use rollback below. The executable and three-second stability checks establish startup only. Inspect startup logs and verify continued health; they do not establish provider behavior or a completed call. Record that evidence and the archive checksum separately from exercised calls. Owner/public routing, PIN protection, incoming screening and native forwarding still require actual Asterisk call tests before claiming production-call parity.

## Roll back to Python

```sh
make rollback TARGET=stefan@192.168.50.83 INSTALL_PREFIX=/opt/baerbel-hs
ssh stefan@192.168.50.83 'journalctl -u phone-assistant --since "5 minutes ago" --no-pager'
```

Rollback removes only the Haskell override, reloads systemd and restarts the same unit using its preserved Python configuration. It checks both services and the same PID across a three-second wait, and prints the actual executable and effective command. It does not guess the original executable from command-line text: confirm the printed path is the preserved Python interpreter. It leaves Haskell releases and the private user JSON available for investigation. It does not rewind shared state or alter Asterisk. Review the printed `ExecStart` to confirm the preserved Python virtual environment is selected, then record separately any real calls exercised after rollback.

Local provider/performance measurements belong in [live-checks.md](live-checks.md), and actual deployment status belongs in [migration.md](migration.md); the presence of this workflow does not mean installation or activation has occurred.
