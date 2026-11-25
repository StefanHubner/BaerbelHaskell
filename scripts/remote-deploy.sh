#!/usr/bin/env bash
set -euo pipefail
action=$1
work=$(cd "$(dirname "$0")" && pwd)
prefix=/home/stefan/apps/baerbel
service=phone-assistant.service
override=/etc/systemd/system/phone-assistant.service.d/zz-baerbel-haskell.conf
user_config=/etc/phone-assistant/user.json

prepare_release() {
  local digest release stage
  digest=$(sha256sum "$work/release.tar.gz")
  digest=${digest%% *}
  release="$prefix/releases/$digest"
  mkdir -p "$prefix/releases"
  if [[ ! -d "$release" ]]; then
    stage=$(mktemp -d "$prefix/releases/.incoming.XXXXXXXX")
    tar --no-same-owner --no-same-permissions -xzf "$work/release.tar.gz" -C "$stage"
    mv "$stage/baerbel" "$release"
    rmdir "$stage"
  fi
  prepare_config "$release/config/global.json"
  env -u LD_PRELOAD ldd "$release/bin/phone-assistant-server"
  "$release/bin/phone-assistant-server" --check "$release/config/global.json" "$work/user.json"
  ln -sfn "releases/$digest" "$prefix/staged"
  printf 'Prepared and checked release: %s\nThe running service has not changed.\n' "$release"
}

prepare_config() {
  local global=$1
  if [[ -f "$user_config" ]]; then
    install -m 0600 "$user_config" "$work/user.json"
  else
    /home/stefan/.venvs/assistant/bin/python "$work/prepare-user-config.py" /etc/phone-assistant/assistant.env "$work/user.json" "$global"
  fi
}

require_idle() {
  local channels
  channels=$(asterisk -rx 'core show channels concise')
  [[ -z "$channels" ]] || { printf 'Asterisk has active calls; the service was not restarted.\n' >&2; return 1; }
}

activate_release() {
  local release service_user unit
  release=$(readlink -e "$prefix/staged")
  service_user=$(systemctl show "$service" --property=User --value)
  install -m 0600 -o "$service_user" -g "$service_user" "$work/user.json" "$user_config"
  runuser -u "$service_user" -- "$release/bin/phone-assistant-server" --check "$release/config/global.json" "$user_config"
  require_idle
  unit=$(mktemp)
  cat > "$unit" <<UNIT
# Managed by Bärbel Haskell deployment; remove this file to restore Python.
[Service]
WorkingDirectory=$release
ExecStart=
ExecStart=$release/bin/phone-assistant-server $release/config/global.json $user_config
UNIT
  install -D -m 0644 "$unit" "$override"
  rm "$unit"
  systemctl daemon-reload
  systemctl restart "$service"
  verify_service "$release/bin/phone-assistant-server"
}

rollback_release() {
  local python
  python=$(readlink -e /home/stefan/.venvs/assistant/bin/python)
  require_idle
  rm -f "$override"
  systemctl daemon-reload
  systemctl restart "$service"
  verify_service "$python"
}

verify_service() {
  local expected=${1:-} first_pid current_pid
  first_pid=$(systemctl show "$service" --property=MainPID --value)
  verify_running "$expected" "$first_pid"
  sleep 3
  current_pid=$(systemctl show "$service" --property=MainPID --value)
  [[ "$current_pid" == "$first_pid" ]] || { printf 'Assistant restarted during startup verification.\n' >&2; return 1; }
  verify_running "$expected" "$current_pid"
  systemctl show "$service" --property=ExecStart --property=MainPID
  printf 'Process checks passed; real call behavior has not been exercised.\n'
}

verify_running() {
  local expected=$1 pid=$2 executable
  systemctl is-active "$service"
  systemctl is-active asterisk.service
  [[ "$pid" =~ ^[1-9][0-9]*$ ]] || { printf 'Assistant has no running main process.\n' >&2; return 1; }
  executable=$(readlink -e "/proc/$pid/exe")
  [[ -z "$expected" || "$executable" == "$expected" ]] || { printf 'Assistant is not running the staged executable.\n' >&2; return 1; }
  printf 'Running executable: %s\n' "$executable"
}

case "$action" in
  prepare) prepare_release ;;
  activate) activate_release ;;
  rollback) rollback_release ;;
esac
