# Bärbel configuration

| Location on the miniserver | Contents |
|---|---|
| `/etc/phone-assistant/global.yaml` | Shared rules, prompts and credentials. The live defaults are `gpt-6.2-sol`, low reasoning, priority service tier and Willow voice. The Git template is `config/global.yaml`. |
| `/var/lib/phone-assistant/users.sqlite3` | Each user’s type, name, registered phone, PIN hash, preferences, SIP accounts, Google connections, memory and delivery history. |
| `/etc/phone-assistant/asterisk.yaml` | Private PBX setup, including routing, network, AMI, handset and Telematica settings. |
| `/etc/asterisk/phone-assistant/` | Generated module files installed by `asteriskconfig`. |
| `/etc/phone-assistant/phone-numbers.json` | Numbering reference data shipped with Bärbel. |

`global.yaml` and `users.sqlite3` both have mode `0600`. Standard users
call the shared line. Pro users have dedicated numbers and incoming-call
handling. Bärbel selects a user by their registered caller number.

## Credentials

The AMI password lets Bärbel connect to local Asterisk. The A&A SMS
password is for PIN and notification texts. OpenAI and Maps use their own
API keys. Google refresh credentials belong to the user in SQLite.
A salted hash verifies the four-digit PIN for protected Gmail tools.

## Connect a Google service

After the user grants consent, import the token export on the miniserver
using the installed server binary.

```sh
phone-assistant-server --connect-google /etc/phone-assistant/global.yaml ACCOUNT SERVICE TOKEN_FILE
```

Use `gmail`, `calendar` or `contacts` for `SERVICE`. Restart Bärbel after
import, then remove the temporary export. Each user gets the Google tools
they have connected.

## Asterisk setup

Standard users share the 033 SIP account in SQLite. Each Pro user has
their own SIP number, username and password there. A user’s registered
caller number is stored separately. `make config`
renders the managed files from the private Asterisk YAML and SQLite after
a SIP account is added. Telematica’s former verification flag was a
record of call testing. The handset extension and Telematica routing live in Asterisk YAML.

## Recovery files

The old Python `assistant.env`, including its Luna and TTS settings, is
archived at `/var/backups/phone-assistant-legacy/assistant.env`. The old
token exports, JSON profile and separate delivery databases are under
`~/apps/baerbel/legacy/`. See [deployment](deployment.md) for recovery
and call tracing.
