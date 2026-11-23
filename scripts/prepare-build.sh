#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cache="${XDG_CACHE_HOME:-$HOME/.cache}/baerbel-hs"
mkdir -p "$cache"
for name in .stack-work .native .tools release; do
  target="$cache/${name#.}"
  if [[ -L "$root/$name" ]]; then
    mkdir -p "$target"
    continue
  fi
  if [[ -d "$root/$name" ]]; then
    mv -T "$root/$name" "$target"
  else
    mkdir -p "$target"
  fi
  ln -s "$target" "$root/$name"
done
