# Architecture source

This diagram describes the Haskell candidate. The existing Python service and Asterisk configuration remain deployed. Local tests are not evidence of a real telephone call or a production installation. Current validation and deployment evidence is recorded in [the migration record](../docs/migration.md).

Edit `BAERBEL_SYSTEM_ARCHITECTURE.dot`, then run `./architecture/render.sh`. The source uses explicit coordinates; preserve the owner/public columns and use Graphviz `neato -n2`. Use DejaVu Sans and inspect both generated views for clipping and crossing labels. Commit the DOT, SVG and PNG together. Do not edit generated views by hand.

The November 2026 functional cleanup uses typed internal commands, prepared provider requests and one shared profile store; the external call paths and deployment boundary are unchanged.

The detailed module and effect boundaries are described in [architecture.md](../docs/architecture.md). Keep policy, call flow and deployment claims consistent across those files. No private numbers, credentials or tokens belong here.
