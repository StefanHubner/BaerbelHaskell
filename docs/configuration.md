# Bärbel configuration

| File | Location | Contents |
|---|---|---|
| Global rules | `config/global.json` in Git. On the server, `~/apps/baerbel/releases/<release>/config/global.json` | Voice and model, reasoning effort, prompts, tools, policies and defaults. |
| Phone reference data | `config/phone-numbers.json` beside the global file | Generated numbering metadata used to validate outbound destinations. |
| Private configuration | `~/.config/baerbel/user.json` for local checks. On the server, `/etc/phone-assistant/user.json` | Service credentials and private account settings. Mode `0600`. |
| Asterisk routing | `deploy/asterisk/account-entry.conf.in` in Git. The prepared cutover updates `/etc/asterisk/phone-assistant-extensions.conf` and saves the private number mapping at `/etc/phone-assistant/account-routes` | Shared and dedicated entry numbers. The existing Asterisk files hold SIP credentials, carrier routes and handset settings. |
| Development checks | `test/live/config.json` | Synthetic caller audio. The production archive excludes it. |

The private server file has one top-level `environment` for service-wide
access such as OpenAI, Google Maps, Asterisk AMI and the SMS sender. With
account routing enabled, the same file has an `accounts` map keyed by a
stable ID. Each entry has `plan` (`Standard` or `Pro`), `name`, `phone` in
international format, and an `environment` for personal settings.

Put the account's Google token references, profile path, calendar and mail
receipt paths, and `PHONE_ASSISTANT_PRIVATE_PIN_HASH` in its personal
`environment`. Each account owns its state. The account configuration does
not expose the voice, model or reasoning effort. Standard accounts use the
shared number. The current dedicated route supports one Pro account and can
use the existing handset if enabled.

On the current server the Google token paths point into
`~/.config/phone-assistant/`. The profile and delivery receipts live under
`/var/lib/phone-assistant/`. The migrated Pro account keeps those paths.
New accounts get separate paths. The account file points to them and
contains their PIN hashes.

SIP accounts and Bärbel user accounts are separate. Standard users share
the old number's SIP endpoint and credentials. The Bristol Pro number
has its own Asterisk endpoint and inbound password. Its SIP To number
selects that endpoint before digest authentication. Asterisk also checks
the carrier's source address. Bärbel selects the individual user only
after Asterisk accepts the call.

The shared inbound SIP settings are in
`/etc/asterisk/phone-assistant-pjsip.conf`. The separate Pro endpoint and its
password live in `/etc/asterisk/phone-assistant-bristol-pjsip.conf` with mode
`0640`. The outbound endpoint and handset settings are in
`/etc/asterisk/phone-assistant-outbound.conf`. Telematica uses its own
`phone-assistant-telematica-pjsip.conf` and
`phone-assistant-telematica-extensions.conf` in the same directory. Bärbel
can request the configured Telematica route for an Austrian destination.

The registered calling number selects an account. The existing keypad PIN
still gates protected Gmail tools. Ordinary conversations and Calendar keep
their existing flow, including Calendar's separate confirmation before
creating an event. The existing PIN hash is carried over during migration.
For a new account, `phone-assistant-server --issue-pin GLOBAL_CONFIG
USER_CONFIG ACCOUNT` generates a four-digit PIN, saves its hash to the private
file and sends the PIN to that account's registered UK mobile. Apply PIN
changes at service start.

Until the privileged cutover, the private candidates are
`~/.cache/baerbel-hs/deploy/accounts.json` and
`~/.cache/baerbel-hs/deploy/account-routes` on the miniserver. The active
service reads its existing private file until `make activate-accounts`
installs the candidates. See [deployment](deployment.md).


Asterisk's routing trace is controlled by `baerbel/account-routing-trace`
in its database. The Bristol repair enables it. It appends only time,
route and outcome to
`/var/lib/phone-assistant-incoming-events/account-routing.log`. The service
account can read the file. Use
`sudo asterisk -rx 'database put baerbel account-routing-trace 0'` on the server
to disable it. For failures before the dialplan, `make trace-carrier`
captures a short redacted A&A SIP summary under `/run`.
