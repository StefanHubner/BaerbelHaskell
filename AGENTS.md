# Code principles

Use explicit Haskell types and Stack with GHCup. Keep functions short and self-contained. Keep computation pure; pass effects as capabilities. Return failures with Either, never throw exceptions or use error, undefined, partial indexing, head, tail or fromJust. Catch operational exceptions only at I/O boundaries; preserve cancellation. Trust internal arguments; parse external input once. Keep nesting to at most three levels (two spaces per level). Reuse functions and higher-order functions instead of duplicating logic or introducing inheritance. Keep rules and defaults in config/global.json and user settings in one private user file. Derive JSON codecs for typed records; keep protocol envelope construction central. No credentials in Git. Preserve all telephone permission boundaries, durable submission uncertainty, timeouts and cancellation semantics.

Run Stack tests, GHC warnings, HLint and Haskell Language Server diagnostics. Report code line changes and the main-function diff. Never replace the running Python service until Haskell behaviour has been verified; list incomplete parity explicitly.

Keep pure values and Either results out of IO unless the operation performs a real effect. Prefer pattern matching and guards to if-then-else. Use automatic derived instances where their representation matches the required format; keep codec options shared and custom parsing limited to actual wire-format differences. Keep the implementation simple and avoid introducing an abstraction framework.

## Functional core and imperative shell

Keep pure feature modules in `src/Baerbel/Core/` (`Baerbel.Core.*`) and environment-facing modules in `src/Baerbel/Runtime/` (`Baerbel.Runtime.*`). Runtime may import Core; Core must never import Runtime. Shared protocol, configuration and failure values belong in Core.

Core accepts and returns ordinary values, including explicit state, events, commands and `Either`/`Maybe` results. Supply time and other observations as values. Core types must not contain `IO`, mutable handles, native resource pointers or effectful callbacks. Capability records such as stores and media interfaces belong in Runtime, even when constructing them is pure. Do not hide effects behind type aliases, polymorphic monad constraints or unsafe execution.

Runtime owns transport, files/databases, clocks, mutable state, concurrency, native codecs, resource lifetimes and effect execution. Keep the underlying business decisions in Core. Preserve atomic updates, cancellation and effect ordering when moving code. Use cohesive feature modules and ordinary data/functions; do not introduce a universal effect framework. Run the core-boundary checks with the normal tests and checks. Keep external Core imports in the reviewed `architecture/core-imports.txt` list; review additions for pure APIs. Missing top-level signatures are compiler errors, so inferred effect bindings cannot bypass the explicit type boundary.

## Configuration ownership and future users

Keep shared application defaults, rules and schemas in `config/global.json`, shared phone-number reference metadata in `config/phone-numbers.json`, and each user's credentials and state paths in their private user configuration. Development-only synthesis settings belong in `test/live/config.json` and stay out of production releases. Standard accounts share the assistant access number and have private memory and integrations. The first Pro account uses a dedicated number for incoming calls. Asterisk owns the handset and carrier bindings. Resolve the account before using personal resources. Keep model, effort and voice application-wide.
