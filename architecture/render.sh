#!/bin/sh
set -eu
cd -- "$(dirname -- "$0")"
neato -n2 -Tsvg BAERBEL_SYSTEM_ARCHITECTURE.dot -o BAERBEL_SYSTEM_ARCHITECTURE.svg
neato -n2 -Tpng BAERBEL_SYSTEM_ARCHITECTURE.dot -o BAERBEL_SYSTEM_ARCHITECTURE.png
