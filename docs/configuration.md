# Bärbel configuration

| Location on the miniserver | Contents |
|---|---|
| `/etc/phone-assistant/global.json` | Shared rules, prompts and credentials. The live defaults are `gpt-6.2-sol`, low reasoning, priority service tier and Willow voice. The Git template is `config/global.json`. |
| `/var/lib/phone-assistant/users.sqlite3` | Each user’s type, name, registered phone, PIN hash, preferences, Google connections, memory and delivery history. |
| `/etc/asterisk/phone-assistant-*.conf` | Phone numbers, SIP credentials, carrier routing and the optional handset. |
| `/etc/phone-assistant/phone-numbers.json` | Numbering reference data shipped with Bärbel. |

`global.json` and `users.sqlite3` both have mode `0600`. Standard users
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
phone-assistant-server --connect-google /etc/phone-assistant/global.json ACCOUNT SERVICE TOKEN_FILE
```

Use `gmail`, `calendar` or `contacts` for `SERVICE`. Restart Bärbel after
import, then remove the temporary export. Each user gets the Google tools
they have connected.

## Asterisk installer inputs

Both files are under `/etc/phone-assistant/`.

- `telematica.env` holds carrier credentials and forwarding settings. The
  installer renders these into Asterisk’s Telematica SIP and dialplan files.
- `account-routes` holds one line in the order `SHARED_DID PRO_DID PRO_ACCOUNT_ID`.
  The routing installer uses it to generate the shared and dedicated routes.

## Recovery files

The old Python `assistant.env`, including its Luna and TTS settings, is
archived at `/var/backups/phone-assistant-legacy/assistant.env`. The old
token exports, JSON profile and separate delivery databases are under
`~/apps/baerbel/legacy/`. See [deployment](deployment.md) for recovery
and call tracing.
