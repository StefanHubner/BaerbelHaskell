# Legacy account migration

The current site installer uses the templates in `config/asterisk/` and
`make config`. See [deployment](../../docs/deployment.md#install-asterisk-settings).

The account entry template is applied to the existing managed Asterisk
dialplan by `make activate-accounts`. The command uses the staged Haskell
release and two private deployment files. It installs the checked dialplan,
reloads it, restarts the assistant, and keeps the previous files for recovery.

- `accounts.json` supplies the account configuration.
- `account-routes` contains the shared DID, the dedicated DID and its account ID
  on one line. Both numbers use international format. The installer keeps a
  copy under `/etc/phone-assistant/`.

The Bristol A&A account sends its called number in the SIP To header.
Asterisk uses that header and a carrier address allowlist to select the
separate Bristol endpoint and its digest password. The shared number keeps
the existing endpoint and password. Run `make install-account-routing` after
staging the corrected release. It verifies the SIP endpoint and identifier
order after restarting idle Asterisk and checks the Haskell executable after
activation. Check both numbers with separate real
calls during cutover. Telematica continues through its existing provider
endpoint and can reach the dedicated account's screening route. The existing
handset can keep its trusted Home route when the dedicated account enables it.

The file format is an operator interface. Add accounts with unique verified
caller numbers and separate state paths. The current dialplan has one dedicated
route and one optional handset. A Standard account has no public screening route.
