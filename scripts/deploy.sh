#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
action=${1:?Expected install, activate or rollback}
target=${2:?Set TARGET to the SSH destination}
prefix=${3:?Set INSTALL_PREFIX to the remote release directory}
archive=${4:-}
case "$action" in
  install) test -f "$archive" ;;
  activate|rollback) ;;
  *) printf 'Unknown deployment action: %s\n' "$action" >&2; exit 2 ;;
esac
stage=$(ssh -- "$target" mktemp -d)
cleanup() {
  local command
  printf -v command 'rm -rf -- %q' "$stage"
  ssh -- "$target" "$command"
}
trap cleanup EXIT
scp -- "$root/scripts/remote-deploy.sh" "$target:$stage/remote-deploy.sh"
case "$action" in
  install) scp -- "$archive" "$target:$stage/release.tar.gz" ;;
esac
printf -v command 'sudo bash %q %q %q %q' "$stage/remote-deploy.sh" "$action" "$prefix" "$stage/release.tar.gz"
ssh -t -- "$target" "$command"
