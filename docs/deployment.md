# Deployment and Python transition

Deployment uses the existing SSH account `stefan@192.168.50.83`. Paths and host are fixed in the scripts; the Make targets take no configuration arguments. Haskell is active in the existing `phone-assistant.service`; Python is preserved for rollback. Remote preparation and activation remain separate steps. Current verification evidence is recorded in [migration.md](migration.md).

Release `1309600e…` from source `8e73e71` is active as of
2026-11-25T23:36Z. Bärbel loaded the split global and instruction files.
Asterisk applied the local site YAML with the shared and Pro SIP entries.
Both services were active with zero restarts after cutover. Production
FastAGI selected both accounts and SQLite passed its integrity checks.
See [migration](migration.md) for the verification record.

## Check and package

Local checks use the private file `~/.config/baerbel/user.json`, prepared from the existing Python configuration. Shared settings live in `config/global.yaml`, with prompts in `config/instructions.yaml` and tool schemas and provider settings in `config/tools/`. `config/phone-numbers.json` supplies reference metadata. Synthetic TTS settings are separate in `test/live/config.json`, used only by the development checker.

```sh
make check
make live-check
make package
```

`make check` runs tests, HLint, HLS and the offline configuration/native check. `make live-check` exercises configured providers and synthetic speech, including OAuth refresh and billed model/audio requests. It does not send messages or create calendar events. The separately requested `make live-delivery-check` sends one labelled self-email and creates, verifies and deletes one private test event each time it runs. See [live-checks.md](live-checks.md) for measured results.

Packaging requires committed source and writes `release/baerbel-linux-x86_64.tar.gz`. The archive contains production commands, global configuration, phone-number reference metadata, license notices and templates, with its source revision and platform recorded in `BUILD.txt`. Credentials, runtime state, the development checker and its TTS configuration stay outside the archive. Build products are kept outside Dropbox under `~/.cache/baerbel-hs/`; prerequisites are described in [the README](../README.md#development-checks) and [native/README.md](../native/README.md).

## Prepare the server without sudo

```sh
make install
```

This uploads the archive and helpers over SSH and installs the checksum release under
`~/apps/baerbel/releases/`. It combines the release template with the private
installation section, stages `global.yaml` with mode `0600`, and runs the
target library and offline executable checks. On the first database
migration it imports the prior account configuration, memory, Google
connections, receipts and cursors into `/var/lib/phone-assistant/users.sqlite3`.
All private data stays on the miniserver.

Remote installation, target library/configuration/native checks and activation passed on 2026-11-25. Post-activation checks confirmed the installed Haskell executable, zero restarts, all three loopback listeners and active Asterisk. The previous Python process exited. See [migration.md](migration.md) for the artifact, timings and verification limits.

## Activate with sudo

```sh
make activate
```

This uses the uploaded helper and requests sudo through the existing SSH connection. The privileged step checks Asterisk for active channels and refuses a busy cutover. The release is already installed under `/home/stefan/apps/baerbel`. This step installs the combined configuration at
`/etc/phone-assistant/global.yaml`, installs `instructions.yaml` and the `tools/` directory alongside it, and repeats the executable's `--check` as
the service user. The first migration stops the assistant before refreshing
the imported state so the database captures completed writes. The former account JSON is archived under
`/var/backups/phone-assistant-users.*`; the source state files are retained
for recovery. The database release reads user state from SQLite.

Activation writes `/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf`, reloads systemd and restarts the existing `phone-assistant.service`. Systemd stops the previous assistant before starting the selected release; no second assistant service is installed. The helper verifies both assistant and Asterisk are active, checks `/proc/MainPID/exe` against the installed executable, waits three seconds and verifies the same PID again. It reports the executable and effective command.

The Python application under `/home/stefan/apps/assistant`, its virtual
environment and its original unit are available for recovery. Asterisk
continues to run the existing dialplan. The active provider and
notification workers use the migrated user database. See
[migration.md](migration.md) for the record of real calls and production
checks.

## Install Asterisk settings

```sh
make config
```

The editable site file is `config/asterisk.yaml`. Keep its private copy
with the checkout. The Make target installs it under
`/etc/phone-assistant/` and runs the Haskell `asteriskconfig` executable
from the staged release. The executable reads the shipped templates and
`/var/lib/phone-assistant/users.sqlite3`. It installs the generated files
in `/etc/asterisk/phone-assistant/` and adds the required module includes.
On a repeat apply it compares the bytes and leaves an up-to-date site
alone. Updates require idle Asterisk and save the replaced files privately.

### A different server

Install and start Asterisk with the stock module configuration and the
PJSIP, AMI, AudioSocket, CDR and MixMonitor modules. Copy the built release,
the private site YAML and a consistent SQLite backup. Adjust the SIP
bind, advertised address, local network and handset ACL in the site YAML.
Provision Bärbel's shared credentials and service separately. The
Asterisk installer does not require a previous Bärbel dialplan.

From the unpacked release on that server:

```sh
sudo install -D -m 0600 config/asterisk.yaml /etc/phone-assistant/asterisk.yaml
sudo bin/asteriskconfig install /etc/phone-assistant/asterisk.yaml \
  /var/lib/phone-assistant/users.sqlite3 config/asterisk \
  "$(sudo mktemp -d /var/backups/baerbel-asterisk.XXXXXXXX)"
```

For a complete installation, create the call-recordings and
incoming-events directories owned by Asterisk with group read access for
the Bärbel service. The regular `make config` deployment already prepares
these directories. Confirm registrations and make a Standard call, a Pro
owner call and a public call to the site’s forwarding route.

## Archive legacy files

```sh
make archive-legacy
```

This moves the standalone OAuth exports, receipt databases, profile
and account staging JSON to
`~/apps/baerbel/legacy/`. The sudo step moves the old Python environment to
`/var/backups/phone-assistant-legacy/assistant.env`. Both archive roots
are private. The archive action checks the running Haskell command line
and database before moving files. It runs while the services continue
handling calls. The Python and account rollback commands restore the
archived files before restarting the corresponding service.

## Trace Asterisk routing

The Asterisk database key `baerbel/account-routing-trace` controls a
redacted trace of time, route and outcome at
`/var/lib/phone-assistant-incoming-events/account-routing.log`. Run
`sudo asterisk -rx 'database put baerbel account-routing-trace 0'` on the
miniserver to disable it. `make trace-carrier` captures a redacted A&A
signalling summary for failures before the dialplan.

## Restore Python

```sh
make rollback
```

After an account routing cutover, `make rollback-accounts` restores the
pre-cutover Asterisk dialplan, private configuration and Haskell systemd
override. It saves a redacted summary of recent Asterisk routing to
`/run/baerbel-account-incident.json` for troubleshooting.

The separate `make rollback` command returns to Python. It uses sudo, checks that Asterisk has no active calls, restores archived legacy files, removes the Haskell override and restarts the same service with its preserved Python configuration. It verifies service health and PID stability and reports the restored executable and command. The archived profile and receipts reflect the state at the database cutover. The current SQLite database stays available for the Haskell release.

Activation, archival of the root-owned Python environment, and rollback require privileged access. No configuration conversion, upload or target binary check needs to be performed manually. See [migration.md](migration.md) for actual deployment status and [live-checks.md](live-checks.md) for provider, synthetic-session and verified real-call evidence.
