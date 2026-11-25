# Architecture source

This diagram describes the Haskell candidate. The existing Python service and Asterisk configuration remain deployed. Deployment and real-call validation are explicitly deferred. Local checks now include real-provider probes and synthetic audio/tool sessions; these do not demonstrate activation or a real telephone call. See [the live-check record](../docs/live-checks.md) for their exact scope and results, and [the migration record](../docs/migration.md) for deployment evidence.

Edit `BAERBEL_SYSTEM_ARCHITECTURE.dot`, then run `./architecture/render.sh`. The source uses explicit coordinates; preserve the owner/public columns and use Graphviz `neato -n2`. Use DejaVu Sans and inspect both generated views for clipping and crossing labels. Commit the DOT, SVG and PNG together. Do not edit generated views by hand.

The implementation is divided by feature between `src/Baerbel/Core/` and `src/Baerbel/Runtime/`. **Functional Core** modules contain immutable values, parsing, decisions and state transitions. **Imperative Shell** modules own capabilities, mutable state, resource lifetimes and external operations. Runtime imports Core; Core never imports Runtime. Some Runtime modules re-export their feature's pure symbols, but the old module paths have been removed.

Solid diagram arrows show call or data flow. The dashed arrow shows the module dependency from `Baerbel.Runtime.*` to `Baerbel.Core.*`; it does not represent a network operation. The core receives values and returns values or decisions. It does not call providers, read files or obtain timestamps.

The architecture tests guard module placement, the Core import boundary and forbidden effect types/operations. Core's reviewed external imports are listed in [core-imports.txt](core-imports.txt). The compiler treats missing top-level signatures as errors. These checks support the boundary; changes to permitted external APIs still require review.

Phone-number validation now belongs to `Core.PhoneNumber`: the global file holds the reference metadata, and startup constructs a pure validator shared by calls. The C++ phone bridge and its protobuf/Abseil dependencies are removed; ICU remains for charset compatibility and WebRTC VAD remains native. Live cleanup stops bridge workers before a bounded provider close exchange, then reports whether closure was acknowledged. The existing PIN gate informs Live of successful local keypad verification before releasing its waiting protected tool.

These changes preserve existing call paths, owner/public permissions and deployment status. The detailed module and effect boundaries are described in [architecture.md](../docs/architecture.md). Keep policy, call flow and deployment claims consistent across those files. No private numbers, credentials or tokens belong here.
