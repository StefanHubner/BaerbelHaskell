# Bärbel configuration

| Location on the miniserver | Contents |
|---|---|
| `/etc/phone-assistant/global.json` | Bärbel's shared rules, prompts, tools, model and voice, plus the private `installation` section for shared OpenAI, Maps, AMI and SMS credentials, logging and the user database location. Mode `0600`. The Git template is `config/global.json`. |
| `/var/lib/phone-assistant/users.sqlite3` | User type, account ID, name, registered phone, PIN hash, individual preferences, Google connections, memory and delivery state. Mode `0600`. The schema is `config/users.sql`. |
| `/etc/phone-assistant/phone-numbers.json` | Read-only numbering reference data shipped with Bärbel. |
| `/etc/asterisk/phone-assistant-*.conf` | SIP credentials, shared and dedicated phone numbers, carrier routing and the optional handset. |
| `test/live/config.json` | Synthetic audio for development checks. It stays out of production. |

Each user has one type, Standard or Pro. Standard users call the shared
number, where Bärbel matches their mobile. Pro has a dedicated number with
incoming-call handling. The current Asterisk configuration supports one
dedicated route. The model, voice, SIP credentials and plan-controlled
permissions are set outside individual user settings.

The database uses separate tables for user types, users, user settings,
Google clients, user grants and scopes, connections to individual Google
services, memory entries, delivery receipts and event cursors. The account
ID scopes queries, and foreign keys prevent a connection from using another
user's OAuth grant. Refresh and per-call caches live in memory within each
account's service instance. Bärbel rejects unsupported account settings.

The shared AMI password lets Bärbel sign in to the local Asterisk Manager
Interface. The shared SMS password authenticates PIN texts and notification
texts with A&A. OpenAI and Maps use their own API keys. Google refresh
credentials and the hashed four-digit PIN belong to each user in SQLite.
SIP and handset authentication live with Asterisk.

The old Python environment, standalone token exports, receipt databases
and profile are recovery files. OpenAI Luna, TTS and STT entries in the
former `assistant.env` belong to that Python configuration. See
[legacy archival](deployment.md#archive-legacy-files) for their locations.

## Connect Google

After consent with the existing authorization helper, import each selected
connection on the miniserver as the service user.

```sh
phone-assistant-server --connect-google /etc/phone-assistant/global.json ACCOUNT gmail TOKEN_FILE
```

Use `calendar` or `contacts` for those connections. A grant covering more
than one service can be imported for each of those services. The database
keeps the validated refresh credentials and granted scopes. Delete the
temporary export after import, then restart Bärbel to load the connection.
A service with no connection for that user stays unavailable.

The registered calling number selects the account. A four-digit keypad
PIN gates protected Gmail tools. Calendar still asks for confirmation
before creating an event. To issue a PIN, run `phone-assistant-server
--issue-pin /etc/phone-assistant/global.json ACCOUNT`. This saves the
salted hash and texts the PIN to the registered UK mobile. A restart
applies a PIN change to running sessions.

## Asterisk

The shared endpoint is in `/etc/asterisk/phone-assistant-pjsip.conf`. Bristol
has separate credentials in `phone-assistant-bristol-pjsip.conf`, selected
by the SIP To number and restricted to the carrier source. The outbound
endpoint and handset settings are in `phone-assistant-outbound.conf`.
Telematica has its own PJSIP and extensions files.

Asterisk's database key `baerbel/account-routing-trace` controls its routing
trace. It writes time, route and outcome to
`/var/lib/phone-assistant-incoming-events/account-routing.log`. Use
`sudo asterisk -rx 'database put baerbel account-routing-trace 0'` on the
miniserver to disable it. `make trace-carrier` captures a redacted A&A
signalling summary for failures before the dialplan.
