# Deployment and Python transition

Deployment uses the existing SSH account `stefan@192.168.50.83`. Paths and host are fixed in the scripts; the Make targets take no configuration arguments. Haskell is active in the existing `phone-assistant.service`; Python is preserved for rollback. Remote preparation and activation remain separate steps. Current verification evidence is recorded in [migration.md](migration.md).

Release `0dd48e38…` from source `06d2f54` is active as of
2026-11-25T22:01:36Z. The service loaded the combined private
`global.json` and normalized SQLite user database. Production FastAGI
selected the Standard and Pro accounts on their respective routes.
Verified PID 229128 with zero restarts and listeners on loopback ports
9092, 9093 and 4573. Asterisk kept PID 226605. The migrated Google grants
refreshed successfully on the miniserver. The current verification used
the FastAGI socket; carrier-call evidence for Bristol comes from the
previous release.

## Check and package

Local checks use the private file `~/.config/baerbel/user.json`, prepared from the existing Python configuration. Shared application rules remain in `config/global.json`; `config/phone-numbers.json` supplies reference metadata beside it. Synthetic TTS settings are separate in `test/live/config.json`, used only by the development checker.

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
`/etc/phone-assistant/global.json` and repeats the executable's `--check` as
the service user. The first migration stops the assistant before refreshing
the imported state so the database captures completed writes. The former account JSON is archived under
`/var/backups/phone-assistant-users.*`; the source state files are retained
for recovery. The database release reads user state from SQLite.

Activation writes `/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf`, reloads systemd and restarts the existing `phone-assistant.service`. Systemd stops the previous assistant before starting the selected release; no second assistant service is installed. The helper verifies both assistant and Asterisk are active, checks `/proc/MainPID/exe` against the installed executable, waits three seconds and verifies the same PID again. It reports the executable and effective command.

The Python application under `/home/stefan/apps/assistant`, its virtual environment, `assistant.env`, original unit, other overrides and shared runtime state remain intact. Asterisk configuration and logrotate are unchanged. Activation starts the enabled provider and notification workers. On an earlier Haskell release, a real owner Gmail call was verified against its executable: both Gmail searches succeeded and provider closure was acknowledged. A known external caller also exercised forwarding/ringing as expected, then hung up before answer; its outcome SMS was accepted. Answered bridging, unknown-caller screening, home-phone behavior and in-call voice notes remain unverified in real calls. Independent inspection of the loaded Asterisk dialplan was unavailable: the current SSH session lacks noninteractive sudo and the AMI account rejects the scoped dialplan query.

## Activate the two account routes

The original JSON account cutover used `make activate-accounts` from the
workstation. This command uses sudo on the miniserver. It checks the staged
private account file and confirms Asterisk is idle before replacing the managed
entry dialplan. It backs up the existing dialplan, private settings and service
override. The command reloads the dialplan and starts the same Haskell service
with the account file. Confirm separate carrier delivery to the two numbers
and check a Standard assistant call and a Pro incoming call after cutover.
See [the routing template notes](../deploy/asterisk/README.md).

## Repair Bristol SIP authentication

The Bristol repair is active as of 2026-11-25 21:08:57 UTC. The first
Bristol attempt reached Asterisk and received repeated `401` responses
after A&A supplied a digest. The two SIP accounts use separate passwords.
The corrected release `75b0c02` loaded both endpoints. The route trace
recorded selection of Bristol and entry into the owner session on a real
call, which the owner confirmed worked. Both running services reported
zero restarts after activation.

If the account split is already active and its SIP routing needs repair,
stage the release and run `make install-account-routing`. The installer preserves the user account
file, splits the inbound SIP credentials and restarts Asterisk only after
checking for active calls. It checks the new endpoint, SIP identifier order,
dialplan and Haskell service. The trace is available to the service account
as described in [the configuration reference](configuration.md).

## Restore Python

```sh
make rollback
```

After an account routing cutover, `make rollback-accounts` restores the
pre-cutover Asterisk dialplan, private configuration and Haskell systemd
override. It saves a redacted summary of recent Asterisk routing to
`/run/baerbel-account-incident.json` for troubleshooting.

The separate `make rollback` command returns to Python. It uses sudo, checks that Asterisk has no active calls, removes only the Haskell override and restarts the same service with its preserved Python configuration. It verifies service health and PID stability and reports the restored executable and command. Releases and the private JSON remain available; shared state is not rewound, so accepted actions are not replayed.

Only activation and rollback require privileged access. No configuration conversion, upload or target binary check needs to be performed manually. See [migration.md](migration.md) for actual deployment status and [live-checks.md](live-checks.md) for provider, synthetic-session and verified real-call evidence.
