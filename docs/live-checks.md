# Live verification

Run the local checks using the private user file at `~/.config/baerbel/user.json`:

```sh
make live-check
```

The executable uses the production Haskell HTTP, OAuth, WebSocket, speech, tool,
PIN and persistence modules. Synthetic caller audio goes through the real
provider and the application's resamplers, queues and playback path. Temporary
profile data and synthetic mailbox results keep the conversation checks isolated
from personal content. Provider reads use the configured accounts; reports expose
counts, timings and fixed statuses rather than their contents or credentials.

| Check | What it exercises |
|---|---|
| Gmail | Actual OAuth refresh, account profile, metadata search and profile/token caching. |
| Calendar | Actual OAuth and complete single-calendar date-range read. |
| Contacts | Actual OAuth, cold warm-up, subsequent search and a bounded directory page. |
| Maps | Public place lookup, reference reuse, details, walking directions and configured elevation. |
| Windguru | Public spot/model discovery, forecast data and cache reuse without changing its retrieval time. |
| Audio | Real TTS, resampling, native VAD, MP3 encoding and decoding, audible content and duration. |
| Greeting | Configured greeting with measured startup-to-audible output. |
| Tool schemas | Actual provider acceptance of the owner tool catalogue. |
| Conversation | Spoken question and contextual follow-up on one connection. |
| Hosted web search | Completed provider web-search item and a spoken answer. |
| Profile tools | Spoken write/read requests through the real dispatcher and a temporary profile file. |
| Background work | A deliberately delayed profile read, an audible reply while it is pending, then its result. |
| Interruption | Caller speech interrupts an actual announcement and the original conversation resumes. |
| Owner hangup | Real model tool delegation, farewell playback, finish-call invocation and session closure. |
| Public screening | Real restricted session and the production decision parser; no owner tools. |
| PIN | Spoken digits remain muted/locked; wrong keypad input is rejected; correct keypad input resumes one tool. |
| Cancellation | An active provider session is cancelled and resources close within the deadline. |

Normal conversation checks require the provider's actual final session event.
The application stops its workers, sends the close request and drains the final
acknowledgement within the configured budget. The report distinguishes provider
finalization from local cleanup. Cancellation retains its own outcome.

Each check has a deadline and reports failure explicitly. Disabled integrations
are identified, rather than counted as successful exchanges. The process exits
unsuccessfully if any enabled check fails. Timing samples describe those runs,
not a latency guarantee or a statistical percentile.

## External delivery

The separately requested delivery checks send one labelled self-email with a
synthetic voice-note attachment, verify the exact sent message and attachment,
and create/read/delete a private temporary calendar event without guests.
They are invoked explicitly:

```sh
make live-delivery-check
```

The normal live-check command does not send email/SMS or create calendar events.
Calendar cleanup is reported separately. A failed or uncertain send is not
automatically replayed. Repeating the delivery command requests a new test email.

## Evidence and deployment boundary

The final integrated run on **2026-11-25 passed all 18 checks**, with zero failures
and zero disabled integrations. All normal sessions reported actual provider
finalization. The rapid-keypad PIN scenario rejected spoken digits and an
incorrect keypad PIN, then executed the original lookup exactly once and spoke
its result after successful verification.

The live runs exposed and verified fixes for two runtime gaps: missing provider
close acknowledgement, and a stale incorrect-PIN prompt suppressing a result
when the caller entered the correct PIN immediately. Verification is now sent
to Live before the pending tool is released. Three deterministic PIN regressions
cover ordering, notification failure and cancellation; seven provider-close
regressions cover acknowledgement, deadlines, failed startup and cancellation.

Selected measurements from the final run on the workstation:

| Measurement | Observed result |
|---|---:|
| Runtime start to audible configured greeting | 3.914 s |
| End of question to next delivered speech | 1.589 s |
| End of contextual follow-up to next delivered speech | 1.040 s |
| Audible acknowledgement during a 20 s delayed tool | 1.368 s after the caller's prompt |
| Caller interruption onset to announcement completion | 2.392 s |
| Active-session cancellation | 0.614 s |
| Local processing of 6.4 s audio | 0.280 s; 0.044 times real time |

These are individual samples, not service-level targets. The report preserves
setup, transcript, delivered-audio, queue and shutdown measurements separately.

The separately authorised delivery checks also passed. Exactly one labelled
self-email was sent, found in the owner's inbox by its returned message ID, and
its MP3 attachment decoded to 6.192 seconds of audible PCM. The original private
calendar event was deleted; the first assertion mistook Google's cancelled-event
record for a failed deletion. Exact-ID readback confirmed deletion, and a
calendar-only repeat passed after correcting that assertion. Both temporary
events are deleted; no second email was sent. The corrected calendar run took
1.484 seconds and verified creation, private visibility, no attendees, readback,
DELETE 204 and the exact cancelled-event record. Nine local cleanup scenarios
also passed.

The final source passed **394 Stack examples**, compiler warnings, HLint with no
hints, HLS across **157 files** with no diagnostics, and the offline configuration
and native-audio check. ShellCheck and strict typing of the metadata utility
passed. Pure phone validation matched all 1,236,669 differential cases and the
864 original fixtures. These checks supplement the live evidence above.

## Real owner Gmail call

On **2026-11-25, approximately 15:58:12–15:59:57 UTC**, the user reported a
successful real owner Gmail call. All **185** associated journal entries identify
PID **214856** and the installed Haskell executable under
`/home/stefan/apps/baerbel/releases/421e7cad8704c7209d4b9137b5fc9e71d455306bfa4f907583c3786c243bdeb6/bin/phone-assistant-server`.
The old Python process is absent, and the service remains at zero restarts.

Two `gmail_search` actions with distinct tool-call IDs both returned `ok`; two
delegation events were recorded. The logs do not map each tool to a delegation
ID. Four backend responses were created and completed; none failed or were
incomplete. Private PIN protection was configured and the keypad prompt was
observed, without recording PIN or transcript contents here. The provider acknowledged
session closure (`finalized=true`). The call journal contained no warning/error
entries. No personal email content, message identifiers or telephone numbers are
included in this record.

| Measurement | Observed result |
|---|---:|
| First Gmail search execution | 1.406 s |
| Second Gmail search execution | 1.061 s |
| Session duration | 105.084 s |
| Recorded input audio | 97.620 s |
| Recorded output audio | 95.060 s |
| Maximum queued audio | 2.860 s |

Search execution timings exclude the preceding PIN wait. These are observations
from this call, not latency guarantees. The evidence
verifies the deployed Haskell owner Gmail path. Other routes and features,
including public screening, native transfers and home-phone behavior, remain
unverified in real calls. Independent inspection of the loaded Asterisk dialplan
was denied by the available SSH/AMI permissions. See [deployment.md](deployment.md)
for the existing-service switch and Python rollback, and [migration.md](migration.md)
for deployment evidence.
