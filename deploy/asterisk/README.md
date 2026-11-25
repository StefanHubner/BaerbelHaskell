The account entry template is applied to the existing managed Asterisk
dialplan by `make activate-accounts`. The command uses the staged Haskell
release and two private deployment files. It installs the checked dialplan,
reloads it, restarts the assistant, and keeps the previous files for recovery.

- `accounts.json` supplies the account configuration.
- `account-routes` contains the shared DID, the dedicated DID and its account ID
  on one line. Both numbers use international format. The installer keeps a
  copy under `/etc/phone-assistant/`.

The A&A incoming SIP target must preserve the called number in the Request-URI
or To URI. A shared contact without a distinct called number is rejected. Use
the existing authenticated A&A endpoint. Check both numbers with separate real
calls during cutover. Telematica continues through its existing provider
endpoint and can reach the dedicated account's screening route. The existing
handset can keep its trusted Home route when the dedicated account enables it.

The file format is an operator interface. Add accounts with unique verified
caller numbers and separate state paths. The current dialplan has one dedicated
route and one optional handset. A Standard account has no public screening route.
