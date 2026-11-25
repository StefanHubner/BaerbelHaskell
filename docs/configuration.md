# Bärbel configuration

| Source in the checkout | Installed location | Contents |
|---|---|---|
| `config/global.yaml` | `/etc/phone-assistant/global.yaml` | Shared application settings. The installed file includes private service credentials. Chat uses `gpt-6.2-sol`, low reasoning and Fast mode (API tier `priority`). |
| `config/instructions.yaml` | `/etc/phone-assistant/instructions.yaml` | Prompts, spoken copy and tool schemas. |
| `config/asterisk.yaml` | `/etc/phone-assistant/asterisk.yaml` | Private site settings for the network, AMI, handset, Telematica and routing. The checkout copy is Git-ignored. |
| `config/asterisk/*.conf.in` | Release `config/asterisk/` | Reusable Asterisk templates. The installer writes the rendered modules under `/etc/asterisk/phone-assistant/`. |
| `config/users.sql` | `/var/lib/phone-assistant/users.sqlite3` | User accounts and SIP assignments, Google authorisations, memory, delivery receipts and the event cursor. |
| `config/phone-numbers.json` | `/etc/phone-assistant/phone-numbers.json` | Shared numbering reference data. |

The private files have mode `0600`. `INCOMING_EVENTS_FILE` points to the
call event feed written by Asterisk. SQLite tracks how far it has been read.

## Asterisk

`make config` copies the checkout's site YAML to `/etc/phone-assistant/`,
then runs `asteriskconfig` against that copy, the release templates and
SQLite. Standard users share one SIP assignment. The existing carrier
uses a separate inbound challenge and outbound authentication for that
line. The site YAML holds the inbound challenge. The database holds the
outbound SIP assignment and each Pro user's dedicated DID and credentials.

`localAccount` associates the handset extension and existing Telematica
and forwarding routes with a Pro account. The installer gets that
account's registered caller number from SQLite. It generates each Pro
endpoint and authentication section from the same template.

## Connect a Google service

After consent, import the token on the miniserver with the installed binary.

```sh
phone-assistant-server --connect-google /etc/phone-assistant/global.yaml ACCOUNT SERVICE TOKEN_FILE
```

Use `gmail`, `calendar` or `contacts` for `SERVICE`. Restart Bärbel after
import and remove the temporary export. Each user gets the Google tools
they have connected. Protected Gmail tools use the existing four-digit PIN.

See [deployment](deployment.md) for installation, recovery and call tracing.
