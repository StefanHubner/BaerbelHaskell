#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
action=$1
target=stefan@192.168.50.83
work=/home/stefan/.cache/baerbel-hs/deploy
case "$action" in
  install)
    ssh -- "$target" "mkdir -p -m 0700 '$work'"
    scp -- "$root/scripts/remote-deploy.sh" "$root/scripts/prepare-user-config.py" "$target:$work/"
    scp -- "$root/release/baerbel-linux-x86_64.tar.gz" "$target:$work/release.tar.gz"
    ssh -- "$target" "bash '$work/remote-deploy.sh' prepare"
    ;;
  rollback-accounts)
    scp -- "$root/scripts/remote-deploy.sh" "$root/scripts/account-incident.py" "$target:$work/"
    ssh -t -- "$target" "sudo bash '$work/remote-deploy.sh' rollback-accounts"
    ;;
  activate|activate-accounts|rollback) ssh -t -- "$target" "sudo bash '$work/remote-deploy.sh' '$action'" ;;
esac
