# Live verification

Later production calls exposed a long-call input overflow and slow responses
across multiple backend tool steps. The audio-pacing and Windguru corrections
passed local checks and focused target-provider reruns. Activation of the
corrected release is pending; the running service still uses the earlier release.

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
| Windguru | Public spot/model discovery, forecast data and cache reuse without changing its retrieval time; saved forecast-set members are checked separately. |
| Audio | Real TTS, resampling, native VAD, MP3 encoding and decoding, audible content and duration. |
| Greeting | Configured greeting with measured startup-to-audible output. |
| Tool schemas | Actual provider acceptance of the owner tool catalogue. |
| Conversation | Spoken question and contextual follow-up on one connection. |
| Sustained audio | At least 210 seconds of continuous telephone input, PCM-duration/wall-clock comparison and a spoken follow-up after 190 seconds; simulated telephone transport. |
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

The pre-deployment integrated run on **2026-11-25 passed all 18 checks**, with zero failures
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

Selected measurements from that run on the workstation:

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

That source passed **394 Stack examples**, compiler warnings, HLint with no
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
verifies the deployed Haskell owner Gmail path. The separate incoming-call test
below has a narrower forwarding result.

## Real known-caller forwarding

On **2026-11-25**, the user tested an external call and reported the expected
redirect/ringing behavior. The Haskell service logged `incoming_classified` with
`caller_class=known` at **16:08:21 UTC**, using the same PID **214856** and deployed
executable. The PBX `incoming-v1` outcome ended at **16:08:40Z** with reason
`CANCEL` and `answered=0`: the caller hung up before an answer.

At **16:08:42 UTC**, Haskell logged `incoming_message_delivery` with status
`sent`, confirming acceptance of the fixed-recipient outcome SMS. This is
submission evidence, not a handset delivery receipt. No error events were found;
the service remained at zero restarts and Asterisk stayed active.

This verifies known-caller forwarding/ringing and the accepted outcome
notification. It does not establish an answered bridge or an owner
outbound/direct call. Answered bridging, unknown-caller screening, home-phone
behavior and in-call voice notes remain unverified in real calls. Independent
inspection of the loaded Asterisk dialplan was denied by the available SSH/AMI
permissions. See [deployment.md](deployment.md) for the existing-service switch
and Python rollback, and [migration.md](migration.md) for deployment evidence.

## Production incidents under investigation

Two later calls on **2026-11-25** ended unexpectedly with `live_input_overflow`,
around **16:14:13 UTC** and **16:17:56 UTC**, each after roughly 149 seconds. No
`end_call` action caused these endings. The Haskell process remained alive as
PID 214856 with zero service restarts; this was a call-session failure. The later
call reported a maximum output queue of **4.380 seconds**. The configured input
buffer holds **250 × 20 ms = 5 seconds**. Relative per-frame scheduling can
accumulate wake-up delay. The corrective source uses absolute frame deadlines
for input and playback, preserves epoch/pause suppression across waits, and adds
a 210-second continuous-input check with a late spoken follow-up. Its telephone
transport is simulated; it does not establish target Asterisk/network behavior.
No corrected production behavior is claimed yet.

The `windguru_forecast_set` tool returned `ok` at **16:12:45 UTC**. Forecast attempts at
**16:13:19/21** returned errors, followed by successful results at **16:13:23/25**.
The logs available for that run did not include the tool error codes. The
corrective source now logs those codes without arguments or results.

A target-server saved-set probe reproduced **15 invalid responses and one API
error across 16 members**. The rejected responses contained **144 timestamps but
150 readings** in some forecast arrays. Both implementations required equal
lengths. The corrected Haskell decoder accepts arrays covering all timestamps,
ignores trailing readings without corresponding timestamps, and still rejects
short arrays. Four additional regressions cover this change and pass in the
final integrated local check.

The actual saved-set tool was rerun on the miniserver after the correction:

| Window | Available members | Provider API errors | Returned points |
|---|---:|---:|---:|
| Current 6 hours | 15 | 1 | 90 |
| Tomorrow, 24 hours | 15 | 1 | 360 |
| Multiple days, 72 hours | 15 | 1 | 480, compacted |

The remaining member returned a provider error with only a message string and
no useful error code. Its cause is undetermined; it is not evidence of a PRO,
access or missing-forecast restriction. These direct target probes do not mean
the corrected release is active in the production service.

Maps requests returned successfully, but the caller experienced excessive wait.
The observed intervals from the final caller transcript event to the first answer
event were **8.207 seconds** and **14.918 seconds**. These are transcript-event
intervals, not measurements of delivered speech. The first `maps_find_place`
execution took **614 ms**. The follow-up executed `profile_set` in **7 ms**,
`maps_find_place` in **243 ms** and `maps_route` in **418 ms**. Several sequential
backend response cycles accounted for most of the remaining wait; the provider
HTTP operations themselves were short in these samples.

The actual deployed Haskell settings and the preserved Python loader both select
`gpt-live-1`, backend `gpt-6-luna`, reasoning effort **`high`**, and a **16,384**
backend output-token limit. Both session builders disable parallel tool calls.
There was no accidental model/effort change during migration. Changing these
settings would be a separate behavior choice; they have not been altered as a
latency workaround.

The final `make check` passed **406 examples with zero failures**, HLint with
**zero hints**, HLS across **159 files with zero failures**, and offline
configuration/native checks. The expanded target suite initially completed with
**17 passes and two failures**: the saved-set issue corrected above, and an
invalid audio-soak assertion. Both failed checks subsequently passed focused
reruns. Google, Maps, native audio, general speech, interruption, PIN, owner
hangup and cancellation passed in the full run.

The first target soak stayed alive for 210 seconds, answered after 190 seconds
and finalized, but its assertion incorrectly counted variable-sized resampler
callbacks as 20 ms frames. The corrected check measures PCM byte duration
against wall time. Its focused target rerun **passed**:

| Measurement | Corrected target soak |
|---|---:|
| Continuous wall time | 210,005.713 ms |
| Input PCM duration | 209,985.167 ms |
| PCM deficit against wall time | 20.546 ms |
| Maximum output queue | 420 ms |
| Late follow-up to first delivered reply | 1,422.239 ms |
| Late follow-up to first transcript | 839.317 ms |
| Provider finalization | Acknowledged |
| Shutdown | 809.678 ms |

This exercises the production Live runtime and actual speech provider on the
target, with simulated telephone transport and continuous input beyond the
observed 149-second failure. It does not replace a real Asterisk call after
activation. The corrected release is installed and checked under `~/apps/baerbel`; activation
is waiting for sudo authentication. See [deployment.md](deployment.md).
