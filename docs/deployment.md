# Deployment and Python transition

Deployment uses the existing SSH account `stefan@192.168.50.83`. Paths and host are fixed in the scripts; the Make targets take no configuration arguments. Haskell is active in the existing `phone-assistant.service`; Python is preserved for rollback. Remote preparation and activation remain separate steps. Current verification evidence is recorded in [migration.md](migration.md).

The corrective release from commit `21d47c7` is installed and passed target preflight. It fixes accumulating audio delay/overflow and Windguru's trailing forecast arrays. Activation stopped because sudo requires a password; the previous Haskell release still serves calls. From the workstation, run:

```sh
make -C ~/Dropbox/baerbel_hs activate
```

The helper checks for active calls before switching. See [the staged release record](migration.md#corrective-release-staged) for exact revisions and verification.

## Check and package

Local checks use the private file `~/.config/baerbel/user.json`, prepared from the existing Python configuration. Shared rules remain in `config/global.json`.

```sh
make check
make live-check
make package
```

`make check` runs tests, HLint, HLS and the offline configuration/native check. `make live-check` exercises configured providers and synthetic speech, including OAuth refresh and billed model/audio requests. It does not send messages or create calendar events. The separately requested `make live-delivery-check` sends one labelled self-email and creates, verifies and deletes one private test event each time it runs. See [live-checks.md](live-checks.md) for measured results.

Packaging requires committed source and writes `release/baerbel-linux-x86_64.tar.gz`. The archive contains compiled commands, global configuration, license notices and templates, with its source revision and platform recorded in `BUILD.txt`. Credentials and runtime state stay outside the archive. Build products are kept outside Dropbox under `~/.cache/baerbel-hs/`; prerequisites are described in [the README](../README.md#development-checks) and [native/README.md](../native/README.md).

## Prepare the server without sudo

```sh
make install
```

This uploads the existing archive and preparation helpers over SSH to `/home/stefan/.cache/baerbel-hs/deploy`, then installs the checksum release directly under `/home/stefan/apps/baerbel/releases/`. It prepares `user.json` with mode `0600` in the private deployment workspace. An existing `/etc/phone-assistant/user.json` is copied; otherwise the server's Python environment parses the readable `/etc/phone-assistant/assistant.env`. The configuration stays on the server, retains existing credential and state paths, and is never printed.

The helper runs `ldd` and the staged Haskell executable's `--check` as `stefan`, the current service user, before selecting `/home/stefan/apps/baerbel/staged`. It prepares the target binary and configuration without sudo, changing the running service, or opening its listeners. Missing dependencies or a failed preflight stop preparation. Asterisk, libsoxr and ffmpeg remain supplied by the server.

Remote installation, target library/configuration/native checks and activation passed on 2026-11-25. Post-activation checks confirmed the installed Haskell executable, zero restarts, all three loopback listeners, active Asterisk and no startup warnings or errors. The previous Python process exited. See [migration.md](migration.md) for the artifact, timings and verification limits.

## Activate with sudo

```sh
make activate
```

This uses the uploaded helper and requests sudo through the existing SSH connection. The privileged step checks Asterisk for active channels and refuses a busy cutover. The release is already installed under `/home/stefan/apps/baerbel`. This step installs the prepared private configuration at `/etc/phone-assistant/user.json`, then repeats the executable's `--check` as the service user.

Activation writes `/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf`, reloads systemd and restarts the existing `phone-assistant.service`. Systemd stops Python before starting Haskell; no second assistant service is installed. The helper verifies both assistant and Asterisk are active, checks `/proc/MainPID/exe` against the installed executable, waits three seconds and verifies the same PID again. It reports the executable and effective command.

The Python application under `/home/stefan/apps/assistant`, its virtual environment, `assistant.env`, original unit, other overrides and shared runtime state remain intact. Asterisk configuration and logrotate are unchanged. Activation starts the enabled provider and notification workers. A subsequent real owner Gmail call was verified against the deployed Haskell executable: both Gmail searches succeeded and provider closure was acknowledged. A known external caller also exercised forwarding/ringing as expected, then hung up before answer; its outcome SMS was accepted. Answered bridging, unknown-caller screening, home-phone behavior and in-call voice notes remain unverified in real calls. Independent inspection of the loaded Asterisk dialplan was unavailable: the current SSH session lacks noninteractive sudo and the AMI account rejects the scoped dialplan query.

## Restore Python

```sh
make rollback
```

Rollback uses sudo, checks that Asterisk has no active calls, removes only the Haskell override and restarts the same service with its preserved Python configuration. It verifies service health and PID stability and reports the restored executable and command. Releases and the private JSON remain available; shared state is not rewound, so accepted actions are not replayed.

Only activation and rollback require privileged access. No configuration conversion, upload or target binary check needs to be performed manually. See [migration.md](migration.md) for actual deployment status and [live-checks.md](live-checks.md) for provider, synthetic-session and verified real-call evidence.
