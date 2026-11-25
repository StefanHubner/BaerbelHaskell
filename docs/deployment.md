# Deployment and Python transition

Deployment uses the existing SSH account `stefan@192.168.50.83`. Paths and host are fixed in the scripts; the Make targets take no configuration arguments. Haskell is active in the existing `phone-assistant.service`; Python is preserved for rollback. Remote preparation and activation remain separate steps. Current verification evidence is recorded in [migration.md](migration.md).

Release `49448f75…` from source `4a41916` is active as of 2026-11-25T22:58:48Z.
Bärbel loaded `global.yaml` and Asterisk loaded its grouped managed directory.
SQLite holds the shared Standard SIP assignment and Stefan’s Pro
assignment. Production FastAGI selected both accounts. A repeat
`make config` archived the replaced inputs with both process IDs unchanged.
See [migration](migration.md) for the verification record.

## Check and package

Local checks use the private file `~/.config/baerbel/user.json`, prepared from the existing Python configuration. Shared application rules remain in `config/global.yaml`; `config/phone-numbers.json` supplies reference metadata beside it. Synthetic TTS settings are separate in `test/live/config.json`, used only by the development checker.

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
installation section, stages `global.json` with mode `0600`, and runs the
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
`/etc/phone-assistant/global.yaml` and repeats the executable's `--check` as
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

The release includes the Haskell `asteriskconfig` executable. It uses
`/etc/phone-assistant/asterisk.yaml` for PBX policy and
`/var/lib/phone-assistant/users.sqlite3` for SIP accounts. The generated
files are grouped in `/etc/asterisk/phone-assistant/` and loaded by the
appropriate Asterisk modules. The installer checks that the PBX is idle
and preserves the replaced files in a private backup.

Both `make install` and `make config` can run from a checkout on the
miniserver. The same targets use SSH from the workstation. A repeated
`make config` compares the rendered bytes and exits when the files and
includes are current. The existing handset extension and Telematica
endpoint and routing rules are part of the private Asterisk YAML.

The initial adoption moves the existing shared and Pro credentials into
SQLite, changes the old internal Pro ID to `stefan`, and archives the
former JSON configuration and `account-routes` file. It preserves the
existing primary Pro routing. Additional Pro SIP accounts generate
dedicated owner entry routes. Standard users use the shared SIP account
without requiring a new Asterisk endpoint.

## Archive legacy files

```sh
make archive-legacy
```

This moves the standalone OAuth exports, receipt databases, profile,
old voice audition files and account staging JSON to
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
