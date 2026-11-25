# Architecture source

The diagram shows the live Asterisk YAML and SQLite-backed SIP layout
activated at 2026-11-25T22:58:48Z. The loaded FastAGI listener matched both
users. A repeat `make config` left Asterisk and Bärbel on the same
processes while archiving the superseded inputs. See the [migration
record](../docs/migration.md) for deployment and real-call evidence.

Edit `BAERBEL_SYSTEM_ARCHITECTURE.dot`, then run `./architecture/render.sh`. The source uses explicit coordinates; preserve the owner/public columns and use Graphviz `neato -n2`. Use DejaVu Sans and inspect both generated views for clipping and crossing labels. Commit the DOT, SVG and PNG together. Do not edit generated views by hand.

The implementation is divided by feature between `src/Baerbel/Core/` and `src/Baerbel/Runtime/`. **Functional Core** modules contain immutable values, parsing, decisions and state transitions. **Imperative Shell** modules own capabilities, mutable state, resource lifetimes and external operations. Runtime imports Core; Core never imports Runtime. Some Runtime modules re-export their feature's pure symbols, but the old module paths have been removed.

Solid diagram arrows show call or data flow. The dashed arrow shows the module dependency from `Baerbel.Runtime.*` to `Baerbel.Core.*`; it does not represent a network operation. The core receives values and returns values or decisions. It does not call providers, read files or obtain timestamps.

The architecture tests guard module placement, the Core import boundary and forbidden effect types/operations. Core's reviewed external imports are listed in [core-imports.txt](core-imports.txt). The compiler treats missing top-level signatures as errors. These checks support the boundary; changes to permitted external APIs still require review.

Phone-number validation now belongs to `Core.PhoneNumber`: `config/phone-numbers.json` holds the shared reference metadata, and startup constructs a pure validator shared by calls. The C++ phone bridge and its protobuf/Abseil dependencies are removed; ICU remains for charset compatibility and WebRTC VAD remains native. Live cleanup stops bridge workers before a bounded provider close exchange, then reports whether closure was acknowledged. The existing PIN gate informs Live of successful local keypad verification before releasing its waiting protected tool.

The corrective source now calculates continuous audio deadlines in `Core.Concurrent`; Runtime reads the clock, waits and preserves pause/preview suppression before delivery. Tool logs add only the failure code to their existing metadata. These changes preserve the diagram topology. Their local and focused target-provider checks passed; they remain included in active release `559d2cdf…` as recorded in [live-checks.md](../docs/live-checks.md).

The source now has one production speech path: Live WebSocket audio with its delegated response backend. The unused STT/chat/TTS pipeline is removed. Production combines the shared policy and credentials in one private `global.yaml`. The database schema in `config/users.sql` covers account types, users, OAuth clients and grants, connections, memory, receipts and cursors. The reference metadata and development TTS fixtures remain separate. The latest route check exercised the running FastAGI listener.

The design preserves existing call paths and owner/public permissions; deployment reuses the existing service. The detailed module and effect boundaries are described in [architecture.md](../docs/architecture.md). Keep policy, call flow and deployment claims consistent across those files. No private numbers, credentials or tokens belong here.

The account deployment adds a shared Standard entry number and a dedicated Pro route on the existing Asterisk service. The diagram shows the deployed routes. FastAGI matches the registered caller number, and AMI supplies the account for each authorized or restricted call. The Bristol owner route has been exercised on a real call.

The Bristol correction gives the shared number and dedicated Pro number separate Asterisk SIP credentials. The routing trace records only the selected route and outcome.

The `asteriskconfig` installer combines private Asterisk YAML with the shared and dedicated SIP accounts in SQLite. It places generated module configuration in one `phone-assistant/` directory.
