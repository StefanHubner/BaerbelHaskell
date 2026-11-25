#!/usr/bin/env bash
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
action=$1
target=stefan@192.168.50.83
work=/home/stefan/.cache/baerbel-hs/deploy
# The same two targets can run directly on the miniserver.
addresses=" $(hostname -I) "
case "$addresses:$action" in
  *" 192.168.50.83 "*:install)
    install -d -m 0700 "$work"
    install -m 0644 "$root/scripts/remote-deploy.sh" "$root/scripts/prepare-user-config.py" "$work/"
    install -m 0644 "$root/release/baerbel-linux-x86_64.tar.gz" "$work/release.tar.gz"
    exec bash "$work/remote-deploy.sh" prepare
    ;;
  *" 192.168.50.83 "*:install-account-routing)
    install -m 0644 "$root/scripts/remote-deploy.sh" "$work/remote-deploy.sh"
    exec sudo bash "$work/remote-deploy.sh" install-account-routing
    ;;
esac

case "$action" in
  install)
    ssh -- "$target" "mkdir -p -m 0700 '$work'"
    scp -- "$root/scripts/remote-deploy.sh" "$root/scripts/prepare-user-config.py" "$target:$work/"
    scp -- "$root/release/baerbel-linux-x86_64.tar.gz" "$target:$work/release.tar.gz"
    ssh -- "$target" "bash '$work/remote-deploy.sh' prepare"
    ;;
  install-account-routing)
    scp -- "$root/scripts/remote-deploy.sh" "$target:$work/"
    ssh -t -- "$target" "sudo bash '$work/remote-deploy.sh' install-account-routing"
    ;;
  archive-legacy)
    scp -- "$root/scripts/remote-deploy.sh" "$target:$work/"
    ssh -- "$target" "bash '$work/remote-deploy.sh' archive-legacy-state"
    ssh -t -- "$target" "sudo bash '$work/remote-deploy.sh' archive-legacy-env"
    ;;
  trace-carrier)
    scp -- "$root/scripts/trace-carrier.py" "$target:$work/baerbel-sip-capture.py"
    ssh -t -- "$target" "sudo python3 '$work/baerbel-sip-capture.py'"
    ;;
  rollback-accounts)
    scp -- "$root/scripts/remote-deploy.sh" "$root/scripts/account-incident.py" "$target:$work/"
    ssh -t -- "$target" "sudo bash '$work/remote-deploy.sh' rollback-accounts"
    ;;
  activate|activate-accounts|rollback) ssh -t -- "$target" "sudo bash '$work/remote-deploy.sh' '$action'" ;;
esac
